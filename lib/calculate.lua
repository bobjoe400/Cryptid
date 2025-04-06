-- calculate.lua: modifications specifically for card calculation

-- deal with Rigged and Fragile when scoring a playing card
local ec = eval_card
function eval_card(card, context)
	if card.will_shatter then
		return {}, {}
	end
	-- Store old probability for later reference
	local ggpn = G.GAME.probabilities.normal
	if card.ability.cry_rigged then
		G.GAME.probabilities.normal = 1e9
	end
	local ret, post = ec(card, context)
	if card.ability.cry_rigged then
		G.GAME.probabilities.normal = ggpn
	end
	return ret, post
end

--some functions to minimize the load on calculate_joker itself
function Card:cry_copy_ability()
	local orig_ability = {}
	if self.ability then
		for i, j in pairs(self.ability) do
			if (type(j) == "table") and is_number(j) then
				orig_ability[i] = to_big(j)
			elseif type(j) == "table" then
				orig_ability[i] = {}
				for i2, j2 in pairs(j) do
					orig_ability[i][i2] = j2
				end
			else
				orig_ability[i] = j
			end
		end
	end
	return orig_ability
end
local cj = Card.calculate_joker

-- Helper: Deep-copy a table.
local function deepCopyTable(orig)
	if type(orig) ~= "table" then
		return orig
	end
	local copy = {}
	for k, v in pairs(orig) do
		copy[k] = deepCopyTable(v)
	end
	return copy
end
  
-- Helper: Get a nested value given a key spec (a table of keys).
local function getNestedValue(t, keySpec)
	if type(t) ~= "table" then
		return nil
	end
	if #keySpec == 1 then
		return t[keySpec[1]]
	end
	if #keySpec == 2 then
		local sub = t[keySpec[1]]
		if type(sub) ~= "table" then
			return nil
		end
		return sub[keySpec[2]]
	end
	return nil
end
  
  -- Helper: Set a nested value given a key spec.
local function setNestedValue(t, keySpec, value)
	if type(t) ~= "table" then
		return
	end
	if #keySpec == 1 then
		t[keySpec[1]] = value
		return
	end
	if #keySpec == 2 then
		local sub = t[keySpec[1]]
		if type(sub) == "table" then
			sub[keySpec[2]] = value
		end
		return
	end
end

-- Helper 1: Check if the ability is blacklisted.
local function isAbilityBlacklisted(abilityName)
	local blacklist = {
		"cry-happyhouse", "Acrobat", "cry-sapling", "cry-mstack",
		"cry-notebook", "Invisible Joker", "cry-Old Invisible Joker"
	}
	for _, name in ipairs(blacklist) do
		if abilityName == name then
			return true
		end
	end
	return false
end

  -- Helper 2: Initialize (or reset) the scaling record for a card.
local function initDoubleScaleRecord(card)
	local cardID = card.sort_id
	local dbl_info = G.GAME.cry_double_scale[cardID]
	if dbl_info and not is_number(dbl_info.scaler_base) then
		return dbl_info
	end
	dbl_info = { ability = { double_scale = true } }
	G.GAME.cry_double_scale[cardID] = dbl_info
	dbl_info.ability = deepCopyTable(card.ability)
	return dbl_info
end
  
-- Helper: Process a top-level field for auto-detection.
local function processTopLevelField(card, dbl_info, k, default_modifiers)
	local v = card.ability[k]
	if k == "extra_value" or not dbl_info.scaler_tracker[k] or dbl_info.ability[k] == v then 
	  return nil
	end
	if not is_number(v) or not is_number(dbl_info.ability[k]) then 
	  return nil
	end

	local predicted_mod = math.abs(to_number(to_big(v)) - to_number(to_big(dbl_info.ability[k])))
	local best_key = { "" }
	local best_coeff = 10^100

	for l, u in pairs(card.ability) do
		if not (default_modifiers[l] and default_modifiers[l] == u) then
			if l ~= k and is_number(u) then
				local ratio = to_number(to_big(predicted_mod / u))
				if ratio >= 0.999 and ratio < best_coeff then
					best_coeff = ratio
					best_key = { l }
				end
			end
			if type(card.ability[l]) == "table" then
				for subKey, subVal in pairs(card.ability[l]) do
					if is_number(subVal) then
						local ratio = to_number(to_big(predicted_mod / subVal))
						if ratio >= 0.999 and ratio < best_coeff then
							best_coeff = ratio
							best_key = { l, subKey }
						end
					end
				end
			end
	  	end
	end
  
	dbl_info.scaler_tracker[k] = true
	return best_key
  end
  
-- Helper: Process a nested field for auto-detection.
local function processNestedField(card, dbl_info, k, subKey)
	local trackerField = k .. "/" .. subKey
	if dbl_info.scaler_tracker[trackerField] then
		return nil 
	end
	local subVal = card.ability[k][subKey]
	if dbl_info.ability[k][subKey] == subVal then
		return nil 
	end
	if not is_number(subVal) or not is_number(dbl_info.ability[k][subKey]) then
		return nil 
	end

	local predicted_mod = math.abs(subVal - dbl_info.ability[k][subKey])
	local best_key = { "" }
	local best_coeff = 10^100

	for l, u in pairs(card.ability) do
		if is_number(u) then
			local ratio = to_number(to_big(predicted_mod / u))
			if ratio >= 0.999 and ratio < best_coeff then
				best_coeff = ratio
				best_key = { l }
			end
		end
		if type(card.ability[l]) ~= "table" then
			goto continue
		end
		for subL, subU in pairs(card.ability[l]) do
			if not (l == k and subL == subKey) and is_number(subU) then
				local ratio = to_number(to_big(predicted_mod / subU))
				if ratio >= 0.999 and ratio < best_coeff then
					best_coeff = ratio
					best_key = { l, subL }
				end
			end
		end
		::continue::
	end

	dbl_info.scaler_tracker[trackerField] = true
	return best_key
end
  
-- Helper: Auto-detect scaling fields.
local function autoDetectScalingFields(card, dbl_info, orig_ability)
	local default_modifiers = {
		mult = 0, h_mult = 0, h_x_mult = 0, h_dollars = 0,
		p_dollars = 0, t_mult = 0, t_chips = 0, x_mult = 1,
		h_size = 0, d_size = 0,
	}

	dbl_info.base = dbl_info.base or {}
	dbl_info.scaler = dbl_info.scaler or {}
	dbl_info.scaler_tracker = dbl_info.scaler_tracker or {}

	local infoIndex = #dbl_info.base
	for k, _ in pairs(card.ability) do
		local best_key = processTopLevelField(card, dbl_info, k, default_modifiers)
		if best_key then
			infoIndex = infoIndex + 1
			dbl_info.base[infoIndex] = { k }
			dbl_info.scaler[infoIndex] = best_key
		end
	end

	for k, v in pairs(card.ability) do
		if not (type(v) == "table" and type(dbl_info.ability[k]) == "table") then
			goto continue
		end
		for subKey, _ in pairs(v) do
			local best_key = processNestedField(card, dbl_info, k, subKey)
			if best_key then
				infoIndex = infoIndex + 1
				dbl_info.base[infoIndex] = { k, subKey }
				dbl_info.scaler[infoIndex] = best_key
			end
		end
		::continue::
	end

	for i = 1, #dbl_info.scaler do
		if not dbl_info.scaler_base or not dbl_info.scaler_base[i] then
			dbl_info.scaler_base = dbl_info.scaler_base or {}
			dbl_info.scaler_base[i] = getNestedValue(orig_ability, dbl_info.scaler[i])
		end
	end
end

-- Helper 3: Set up the scaling configuration.
local function setupScalingConfiguration(card, dbl_info, orig_ability)
	local abilityName = card.ability.name

	-- Let mods supply custom scaling info if available.
	if type(card.config.center.cry_double_scale_info) == "function" then
		if not dbl_info.scaler then
			card.config.center:cry_double_scale_info(card, dbl_info)
		end
		return
	end

	if abilityName == "cry-Number Blocks" then
		dbl_info.base = { { "extra", "money" } }
		dbl_info.scaler = { { "extra", "money_mod" } }
		dbl_info.scaler_base = dbl_info.scaler_base or { card.ability.extra.money_mod }
		return
	end
	if abilityName == "cry-Redeo" then
		dbl_info.base = { { "extra", "money_req" } }
		dbl_info.scaler = { { "extra", "money_mod" } }
		dbl_info.scaler_base = dbl_info.scaler_base or { card.ability.extra.money_mod }
		return
	end
	if abilityName == "cry-Chili Pepper" then
		dbl_info.base = { { "extra", "Xmult" } }
		dbl_info.scaler = { { "extra", "Xmult_mod" } }
		dbl_info.scaler_base = dbl_info.scaler_base or { card.ability.extra.Xmult_mod }
		return
	end
	if abilityName == "cry-Scalae" then
		dbl_info.base = { { "extra", "scale" } }
		dbl_info.scaler = { { "extra", "scale_mod" } }
		dbl_info.scaler_base = dbl_info.scaler_base or { card.ability.extra.shadow_scale_mod }
		return
	end
	if abilityName == "cry-mprime" then
		dbl_info.base = { { "extra", "mult" } }
		dbl_info.scaler = { { "extra", "bonus" } }
		dbl_info.scaler_base = dbl_info.scaler_base or { card.ability.extra.bonus }
		return
	end
	if abilityName == "Yorick" then
		dbl_info.base = { { "x_mult" } }
		dbl_info.scaler = { { "extra", "xmult" } }
		dbl_info.scaler_base = dbl_info.scaler_base or { 1 }
		return
	end
	if abilityName == "Hologram" then
		dbl_info.base = { { "x_mult" } }
		dbl_info.scaler = { { "extra" } }
		dbl_info.scaler_base = dbl_info.scaler_base or { card.ability.extra }
		return
	end
	if abilityName == "Gift Card" then
		dbl_info.base = { { "extra_value" } }
		dbl_info.scaler = { { "extra" } }
		dbl_info.scaler_base = dbl_info.scaler_base or { card.ability.extra }
		return
	end
	if abilityName == "Egg" then
		dbl_info.base = { { "extra_value" } }
		dbl_info.scaler = { { "extra" } }
		dbl_info.scaler_base = dbl_info.scaler_base or { card.ability.extra }
		return
	end

	autoDetectScalingFields(card, dbl_info, orig_ability)
end
  
-- Helper 4: Apply the scaling updates to affected cards.
local function applyScalingUpdates(card, dbl_info, orig_ability)
	if not (dbl_info.scaler and #dbl_info.scaler > 0) then 
		return 
	end

	for i = 1, #dbl_info.scaler do
		local orig_scale_base = getNestedValue(orig_ability, dbl_info.base[i])
		local orig_scale_scale = getNestedValue(orig_ability, dbl_info.scaler[i])
		if not (orig_scale_base and orig_scale_scale) then 
			goto continue 
		end

		local new_scale_base = getNestedValue(card.ability, dbl_info.base[i])
		local true_base = dbl_info.scaler_base[i]
		if not (new_scale_base and to_big(math.abs(new_scale_base - orig_scale_base)) > to_big(0)) then 
			goto continue 
		end

		for _, joker in ipairs(G.GAME.scale_mod_jokers or {}) do
			local center = joker.config.center
			local origProb = G.GAME.probabilities.normal
			if joker.ability.cry_rigged then
				G.GAME.probabilities.normal = 1e9
			end
			local o = center:cry_scale_mod(joker, card, orig_scale_scale, true_base, orig_scale_base, new_scale_base)
			if joker.ability.cry_rigged then
				G.GAME.probabilities.normal = origProb
			end
			if o then
				setNestedValue(card.ability, dbl_info.scaler[i], o)
				card_eval_status_text(joker, "extra", nil, nil, nil, { message = localize("k_upgrade_ex") })
			end
		end
		::continue::
	end
end
  
-- Helper: Update the cached scaling jokers.
function Cryptid.update_scale_mod_jokers()
	G.GAME.scale_mod_jokers = {}
	for i, joker in ipairs(G.jokers.cards) do
		if joker.config.center.cry_scale_mod and type(joker.config.center.cry_scale_mod) == "function" then
			table.insert(G.GAME.scale_mod_jokers, joker)
		end
	end
end
  
  -- Main function: Delegates to the helper functions.
function Card:cry_double_scale_calc(orig_ability, in_context_scaling)
	if isAbilityBlacklisted(self.ability.name) then 
		return 
	end
	if not (self.ability and type(self.ability) == "table") then 
		return 
	end

	local dbl_info = initDoubleScaleRecord(self)
	setupScalingConfiguration(self, dbl_info, orig_ability)
	applyScalingUpdates(self, dbl_info, orig_ability)
end

local smcc = SMODS.calculate_context
function SMODS.calculate_context(context, return_table)
	for k, v in pairs(SMODS.Events) do
		if G.GAME.events and G.GAME.events[k] then
			context.pre_jokers = true
			v:calculate(context)
			context.pre_jokers = nil
		end
	end
	if context.using_consumeable then
		local _card = context.consumeable
		--calculate the joker effects
		local eval, post = eval_card(_card, context)
		local effects = { eval }
		for _, v in ipairs(post) do
			effects[#effects + 1] = v
		end

		if context.other_joker then
			for k, v in pairs(effects[1]) do
				v.other_card = _card
			end
		end
		if effects[1].retriggers then
			context.retrigger_joker = true
			for rt = 1, #effects[1].retriggers do
				context.retrigger_joker = effects[1].retriggers[rt].retrigger_card
				local rt_eval, rt_post = eval_card(_card, context)
				table.insert(effects, { effects[1].retriggers[rt] })
				table.insert(effects, rt_eval)
				for _, v in ipairs(rt_post) do
					effects[#effects + 1] = v
				end
			end
			context.retrigger_joker = false
		end
		if return_table then
			for _, v in ipairs(effects) do
				if v.jokers and not v.jokers.card then
					v.jokers.card = _card
				end
				return_table[#return_table + 1] = v
			end
		else
			SMODS.trigger_effects(effects, _card)
		end
	end
	local ret = smcc(context, return_table)
	for k, v in pairs(SMODS.Events) do
		if G.GAME.events and G.GAME.events[k] then
			context.post_jokers = true
			v:calculate(context)
			context.post_jokers = nil
		end
	end
	return ret
end

function Card:calculate_joker(context)
	local active_side = self
	if
		next(find_joker("cry-Flip Side"))
		and not context.dbl_side
		and self.edition
		and self.edition.cry_double_sided
	then
		self:init_dbl_side()
		active_side = self.dbl_side
		if context.callback then
			local m = context.callback
			context.callback = function(card, a, b)
				m(self, a, b)
			end
			context.dbl_side = true
		end
	end
	if active_side.will_shatter then
		return
	end
	local ggpn = G.GAME.probabilities.normal
	if not G.GAME.cry_double_scale then
		G.GAME.cry_double_scale = { double_scale = true } --doesn't really matter what's in here as long as there's something
	end
	if active_side.ability.cry_rigged then
		G.GAME.probabilities.normal = 1e9
	end
	local orig_ability = active_side:cry_copy_ability()
	local in_context_scaling = false
	local callback = context.callback
	if active_side.ability.cry_possessed then
		if
			not (
				(context.individual and not context.repetition)
				or context.joker_main
				or (context.other_joker and not context.post_trigger)
			)
		then
			return
		end
		context.callback = nil
	end
	local ret, trig = cj(active_side, context)
	if active_side.ability.cry_possessed and ret then
		if ret.mult_mod then
			ret.mult_mod = ret.mult_mod * -1
		end
		if ret.Xmult_mod then
			ret.Xmult_mod = ret.Xmult_mod ^ -1
		end
		if ret.mult then
			ret.mult = ret.mult * -1
		end
		if ret.x_mult then
			ret.x_mult = ret.x_mult ^ -1
		end
		ret.e_mult = nil
		ret.ee_mult = nil
		ret.eee_mult = nil
		ret.hyper_mult = nil
		ret.Emult_mod = nil
		ret.EEmult_mod = nil
		ret.EEEmult_mod = nil
		ret.hypermult_mod = nil
		if ret.chip_mod then
			ret.chip_mod = ret.chip_mod * -1
		end
		if ret.Xchip_mod then
			ret.Xchip_mod = ret.Xchip_mod ^ -1
		end
		if ret.chips then
			ret.chips = ret.chips * -1
		end
		if ret.x_chips then
			ret.x_chips = ret.x_chips ^ -1
		end
		ret.e_chips = nil
		ret.ee_chips = nil
		ret.eee_chips = nil
		ret.hyper_chips = nil
		ret.Echip_mod = nil
		ret.EEchip_mod = nil
		ret.EEEchip_mod = nil
		ret.hyperchip_mod = nil
		if ret.message then
			-- TODO - this is a hacky way to do this, but it works for now
			if type(ret.message) == "table" then
				ret.message = ret.message[1]
			end
			if ret.message:sub(1, 1) == "+" then
				ret.message = "-" .. ret.message:sub(2)
			elseif ret.message:sub(1, 1) == "X" then
				ret.message = "/" .. ret.message:sub(2)
			else
				ret.message = ret.message .. "?"
			end
		end
		callback(context.blueprint_card or self, ret, context.retrigger_joker)
	end
	if not context.blueprint and (active_side.ability.set == "Joker") and not active_side.debuff then
		if ret or trig then
			in_context_scaling = true
		end
	end
	if active_side.ability.cry_rigged then
		G.GAME.probabilities.normal = ggpn
	end
	active_side:cry_double_scale_calc(orig_ability, in_context_scaling)
	return ret, trig
end

function Cryptid.exponentia_scale_mod(self, orig_scale_scale, orig_scale_base, new_scale_base)
	local jkr = self
	local dbl_info = G.GAME.cry_double_scale[jkr.sort_id]
	if jkr.ability and type(jkr.ability) == "table" then
		if not G.GAME.cry_double_scale[jkr.sort_id] or not G.GAME.cry_double_scale[jkr.sort_id].ability then
			if not G.GAME.cry_double_scale[jkr.sort_id] then
				G.GAME.cry_double_scale[jkr.sort_id] = { ability = { double_scale = true } }
			end
			for k, v in pairs(jkr.ability) do
				if type(jkr.ability[k]) ~= "table" then
					G.GAME.cry_double_scale[jkr.sort_id].ability[k] = v
				else
					G.GAME.cry_double_scale[jkr.sort_id].ability[k] = {}
					for _k, _v in pairs(jkr.ability[k]) do
						G.GAME.cry_double_scale[jkr.sort_id].ability[k][_k] = _v
					end
				end
			end
		end
		if G.GAME.cry_double_scale[jkr.sort_id] and not G.GAME.cry_double_scale[jkr.sort_id].scaler then
			dbl_info.base = { "extra", "Emult" }
			dbl_info.scaler = { "extra", "Emult_mod" }
			dbl_info.scaler_base = jkr.ability.extra.Emult_mod
			dbl_info.offset = 1
		end
	end
	local true_base = dbl_info.scaler_base
	if true_base then
		for i = 1, #G.jokers.cards do
			local obj = G.jokers.cards[i].config.center
			if obj.cry_scale_mod and type(obj.cry_scale_mod) == "function" then
				local ggpn = G.GAME.probabilities.normal
				if G.jokers.cards[i].ability.cry_rigged then
					G.GAME.probabilities.normal = 1e9
				end
				local o = obj:cry_scale_mod(
					G.jokers.cards[i],
					jkr,
					orig_scale_scale,
					true_base,
					orig_scale_base,
					new_scale_base
				)
				if G.jokers.cards[i].ability.cry_rigged then
					G.GAME.probabilities.normal = ggpn
				end
				if o then
					if #dbl_info.scaler == 2 then
						if
							not (
								not jkr.ability[dbl_info.scaler[1]]
								or not jkr.ability[dbl_info.scaler[1]][dbl_info.scaler[2]]
							)
						then
							jkr.ability[dbl_info.scaler[1]][dbl_info.scaler[2]] = o
							orig_scale_scale = o
						end
					else
						if jkr.ability[dbl_info.scaler[1]] then
							jkr.ability[dbl_info.scaler[1]] = o
							orig_scale_scale = o
						end
					end
					card_eval_status_text(
						G.jokers.cards[i],
						"extra",
						nil,
						nil,
						nil,
						{ message = localize("k_upgrade_ex") }
					)
				end
				local reps = {}
				for i2 = 1, #G.jokers.cards do
					local _card = G.jokers.cards[i2]
					local ggpn = G.GAME.probabilities.normal
					if _card.ability.cry_rigged then
						G.GAME.probabilities.normal = 1e9
					end
					local check =
						cj(G.jokers.cards[i2], { retrigger_joker_check = true, other_card = G.jokers.cards[i] })
					if _card.ability.cry_rigged then
						G.GAME.probabilities.normal = ggpn
					end
					if type(check) == "table" then
						reps[i2] = check and check.repetitions and check or 0
					else
						reps[i2] = 0
					end
					if
						G.jokers.cards[i2] == G.jokers.cards[i]
						and G.jokers.cards[i].edition
						and G.jokers.cards[i].edition.retriggers
					then
						local old_repetitions = reps[i] ~= 0 and reps[i].repetitions or 0
						local check = false --G.jokers.cards[i]:calculate_retriggers()
						if check and check.repetitions then
							check.repetitions = check.repetitions + old_repetitions
							reps[i] = check
						end
					end
				end
				for i0, j in ipairs(reps) do
					if (type(j) == "table") and j.repetitions and (j.repetitions > 0) then
						for r = 1, j.repetitions do
							card_eval_status_text(j.card, "jokers", nil, nil, nil, j)
							local ggpn = G.GAME.probabilities.normal
							if G.jokers.cards[i].ability.cry_rigged then
								G.GAME.probabilities.normal = 1e9
							end
							local o = obj:cry_scale_mod(
								G.jokers.cards[i],
								jkr,
								orig_scale_scale,
								true_base,
								orig_scale_base,
								new_scale_base
							)
							if G.jokers.cards[i].ability.cry_rigged then
								G.GAME.probabilities.normal = ggpn
							end
							if o then
								if #dbl_info.scaler == 2 then
									if
										not (
											not jkr.ability[dbl_info.scaler[1]]
											or not jkr.ability[dbl_info.scaler[1]][dbl_info.scaler[2]]
										)
									then
										jkr.ability[dbl_info.scaler[1]][dbl_info.scaler[2]] = o
										orig_scale_scale = o
									end
								else
									if jkr.ability[dbl_info.scaler[1]] then
										jkr.ability[dbl_info.scaler[1]] = o
										orig_scale_scale = o
									end
								end
								card_eval_status_text(
									G.jokers.cards[i],
									"extra",
									nil,
									nil,
									nil,
									{ message = localize("k_upgrade_ex") }
								)
							end
						end
					end
				end
			end
		end
	end
end

function Cryptid.compound_interest_scale_mod(self, orig_scale_scale, orig_scale_base, new_scale_base)
	local jkr = self
	local dbl_info = G.GAME.cry_double_scale[jkr.sort_id]
	if jkr.ability and type(jkr.ability) == "table" then
		if not G.GAME.cry_double_scale[jkr.sort_id] or not G.GAME.cry_double_scale[jkr.sort_id].ability then
			if not G.GAME.cry_double_scale[jkr.sort_id] then
				G.GAME.cry_double_scale[jkr.sort_id] = { ability = { double_scale = true } }
			end
			for k, v in pairs(jkr.ability) do
				if type(jkr.ability[k]) ~= "table" then
					G.GAME.cry_double_scale[jkr.sort_id].ability[k] = v
				else
					G.GAME.cry_double_scale[jkr.sort_id].ability[k] = {}
					for _k, _v in pairs(jkr.ability[k]) do
						G.GAME.cry_double_scale[jkr.sort_id].ability[k][_k] = _v
					end
				end
			end
		end
		if G.GAME.cry_double_scale[jkr.sort_id] and not G.GAME.cry_double_scale[jkr.sort_id].scaler then
			dbl_info.base = { "extra", "percent" }
			dbl_info.scaler = { "extra", "percent_mod" }
			dbl_info.scaler_base = jkr.ability.extra.percent_mod
			dbl_info.offset = 1
		end
	end
	local true_base = dbl_info.scaler_base
	if true_base then
		for i = 1, #G.jokers.cards do
			local obj = G.jokers.cards[i].config.center
			if obj.cry_scale_mod and type(obj.cry_scale_mod) == "function" then
				local ggpn = G.GAME.probabilities.normal
				if G.jokers.cards[i].ability.cry_rigged then
					G.GAME.probabilities.normal = 1e9
				end
				local o = obj:cry_scale_mod(
					G.jokers.cards[i],
					jkr,
					orig_scale_scale,
					true_base,
					orig_scale_base,
					new_scale_base
				)
				if G.jokers.cards[i].ability.cry_rigged then
					G.GAME.probabilities.normal = ggpn
				end
				if o then
					if #dbl_info.scaler == 2 then
						if
							not (
								not jkr.ability[dbl_info.scaler[1]]
								or not jkr.ability[dbl_info.scaler[1]][dbl_info.scaler[2]]
							)
						then
							jkr.ability[dbl_info.scaler[1]][dbl_info.scaler[2]] = o
							orig_scale_scale = o
						end
					else
						if jkr.ability[dbl_info.scaler[1]] then
							jkr.ability[dbl_info.scaler[1]] = o
							orig_scale_scale = o
						end
					end
					card_eval_status_text(
						G.jokers.cards[i],
						"extra",
						nil,
						nil,
						nil,
						{ message = localize("k_upgrade_ex") }
					)
				end
				local reps = {}
				for i2 = 1, #G.jokers.cards do
					local _card = G.jokers.cards[i2]
					local ggpn = G.GAME.probabilities.normal
					if _card.ability.cry_rigged then
						G.GAME.probabilities.normal = 1e9
					end
					local check =
						cj(G.jokers.cards[i2], { retrigger_joker_check = true, other_card = G.jokers.cards[i] })
					if _card.ability.cry_rigged then
						G.GAME.probabilities.normal = ggpn
					end
					if type(check) == "table" then
						reps[i2] = check and check.repetitions and check or 0
					else
						reps[i2] = 0
					end
					if
						G.jokers.cards[i2] == G.jokers.cards[i]
						and G.jokers.cards[i].edition
						and G.jokers.cards[i].edition.retriggers
					then
						local old_repetitions = reps[i] ~= 0 and reps[i].repetitions or 0
						local check = false --G.jokers.cards[i]:calculate_retriggers()
						if check and check.repetitions then
							check.repetitions = check.repetitions + old_repetitions
							reps[i] = check
						end
					end
				end
				for i0, j in ipairs(reps) do
					if (type(j) == "table") and j.repetitions and (j.repetitions > 0) then
						for r = 1, j.repetitions do
							card_eval_status_text(j.card, "jokers", nil, nil, nil, j)
							local ggpn = G.GAME.probabilities.normal
							if G.jokers.cards[i].ability.cry_rigged then
								G.GAME.probabilities.normal = 1e9
							end
							local o = obj:cry_scale_mod(
								G.jokers.cards[i],
								jkr,
								orig_scale_scale,
								true_base,
								orig_scale_base,
								new_scale_base
							)
							if G.jokers.cards[i].ability.cry_rigged then
								G.GAME.probabilities.normal = ggpn
							end
							if o then
								if #dbl_info.scaler == 2 then
									if
										not (
											not jkr.ability[dbl_info.scaler[1]]
											or not jkr.ability[dbl_info.scaler[1]][dbl_info.scaler[2]]
										)
									then
										jkr.ability[dbl_info.scaler[1]][dbl_info.scaler[2]] = o
										orig_scale_scale = o
									end
								else
									if jkr.ability[dbl_info.scaler[1]] then
										jkr.ability[dbl_info.scaler[1]] = o
										orig_scale_scale = o
									end
								end
								card_eval_status_text(
									G.jokers.cards[i],
									"extra",
									nil,
									nil,
									nil,
									{ message = localize("k_upgrade_ex") }
								)
							end
						end
					end
				end
			end
		end
	end
end
