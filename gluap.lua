--[[
  An attempt at a genetic programming library which will eventually
  include:

  * Push3 interpreter
  * Actual crossover...
  * Some decent examples...
  * Automatic save/resume?
  * Unit tests...
  Oisín Mac Fhearaí (gmail.denpashogai)

  program ::= instruction | literal | ( program* )
--]]

module(..., package.seeall)

-- maybe the allowed instruction set can be constructed from the
-- complete/generic instructions, but this seems neater.
allowed_instructions = {
  'integer.+', 'integer.-', 'integer.*', 'integer./', 'integer.<',
  'integer.>', 'integer.=', 'integer.max', 'integer.fromboolean',
  'integer.dup', 'integer.pop', 'integer.define', 'integer.flush',
  'float.+', 'float.-', 'float.*', 'float./', 'float.<',
  'float.>', 'float.=', 'float.max', 'float.fromboolean',
  'float.dup', 'float.pop', 'float.define', 'float.flush',
  'boolean.or', 'boolean.and', 'boolean.dup', 'boolean.pop',
  'boolean.define', 'boolean.flush',
  'code.quote', 'code.do', 'code.if', 'code.fromboolean',
  'code.frominteger', 'code.fromfloat', 'code.fromname', 'code.dup',
  'code.pop', 'code.do*range', 'code.define', 'code.flush',
  'exec.if', 'exec.y', 'exec.pop', 'exec.dup', 'exec.do*range',
  'exec.define', 'exec.flush',
}
-- the implemented instructions, including ones that are somewhat generic
instructions = {
  ['+'] = function(env, stack)
    stack = stack or 'integer'
    if env.depth(stack) < 2 then return end
    local b = env.pop(stack)
    local a = env.pop(stack)
    env.push(stack, a+b)
  end,
  ['-'] = function(env, stack)
    stack = stack or 'integer'
    if env.depth(stack) < 2 then return end
    local b = env.pop(stack)
    local a = env.pop(stack)
    env.push(stack, a-b)
  end,
  ['*'] = function(env, stack)
    stack = stack or 'integer'
    if env.depth(stack) < 2 then return end
    local b = env.pop(stack)
    local a = env.pop(stack)
    env.push(stack, a*b)
  end,
  ['/'] = function(env, stack)
    stack = stack or 'integer'
    if env.depth(stack) < 2 then return end
    local b = env.pop(stack)
    local a = env.pop(stack)
    env.push(stack, a/b)
  end,
  ['<'] = function(env, stack)
    stack = stack or 'integer'
    if env.depth(stack) < 2 then return end
    local b = env.pop(stack)
    local a = env.pop(stack)
    local result = a<b
    env.push('boolean', result)
  end,
  ['>'] = function(env, stack)
    stack = stack or 'integer'
    if env.depth(stack) < 2 then return end
    local b = env.pop(stack)
    local a = env.pop(stack)
    local result = a>b
    env.push('boolean', result)
  end,
  ['='] = function(env, stack)
    stack = stack or 'integer'
    if env.depth(stack) < 2 then return end
    local b = env.pop(stack)
    local a = env.pop(stack)
    local result = a==b
    env.push('boolean', result)
  end,
  ['max'] = function(env, stack)
    stack = stack or 'integer'
    if env.depth(stack) < 2 then return end
    local b = env.pop(stack)
    local a = env.pop(stack)
    if a>b then
      env.push(stack, a)
    else
      env.push(stack, b)
    end
  end,
  ['integer.fromboolean'] = function(env)
    if env.depth('boolean') < 1 then return end
    if env.pop('boolean') then
      env.push('integer', 1)
    end
  end,
  ['float.fromboolean'] = function(env)
    if env.depth('boolean') < 1 then return end
    if env.pop('boolean') then
      env.push('float', 1)
    end
  end,
  -- generic stack manipulation
  ['dup'] = function(env, stack)
    if env.depth(stack) < 1 then return end
    env.dup(stack)
  end,
  ['pop'] = function(env, stack)
    if env.depth(stack) < 1 then return end
    env.pop(stack)
  end,
  ['flush'] = function(env, stack)
    env.flush(stack)
  end,

  ['boolean.or'] = function(env)
    if env.depth('boolean') < 2 then return end
    local b = env.pop('boolean')
    local a = env.pop('boolean')
    env.push('boolean', a or b)
  end,
  ['boolean.and'] = function(env)
    if env.depth('boolean') < 2 then return end
    local b = env.pop('boolean')
    local a = env.pop('boolean')
    env.push('boolean', a and b)
  end,

  ['code.quote'] = function(env)
    if env.depth('code') < 1 then return end
    local code = env.pop('exec')
    env.push('code', code)
  end,
  ['code.do'] = function(env)
    if env.depth('code') < 1 then return end
    local code = env.pop('code')
    env.push('exec', code)
  end,
  ['code.if'] = function(env)
    if env.depth('code') < 2 or env.depth('boolean') == 0 then return end
    local x = env.pop('boolean')
    local b = env.pop('code')
    local a = env.pop('code')
    if x then
      env.push('exec', a)
    else
      env.push('exec', b)
    end
  end,
  ['code.fromboolean'] = function(env)
    if env.depth('boolean') < 1 then return end
    env.push('code', env.pop('boolean'))
  end,
  ['code.frominteger'] = function(env)
    if env.depth('integer') < 1 then return end
    env.push('code', env.pop('integer'))
  end,
  ['code.fromfloat'] = function(env)
    if env.depth('float') < 1 then return end
    env.push('code', env.pop('float'))
  end,
  ['code.fromname'] = function(env)
    if env.depth('name') < 1 then return end
    env.push('code', env.pop('name'))
  end,
  -- arguments are reversed!
  ['exec.if'] = function(env)
    if env.depth('exec') < 2 or env.depth('boolean') == 0 then return end
    local x = env.pop('boolean')
    local a = env.pop('exec')
    local b = env.pop('exec')
    if x then
      env.push('exec', a)
    else
      env.push('exec', b)
    end
  end,
  -- Y combinator?
  ['exec.y'] = function(env)
    if env.depth('exec') < 1 then return end
    local x = env.pop('exec')

    local recursion = {{'EXEC.Y', x}}
    env.push('exec', recursion)
    env.push('exec', x)
  end,
  ['do*range'] = function(env, stack)
    if env.depth(stack) < 1 or env.depth('integer') < 2 then return end
    local x = env.pop(stack)
    local destination_index = env.pop('integer')
    local current_index = env.pop('integer')

    env.push('integer', current_index)
    if destination_index == current_index then
      env.push('exec', x)
    else
      if current_index > destination_index then
        current_index = current_index - 1
      else
        current_index = current_index + 1
      end
      local recursion = {{tostring(current_index), tostring(destination_index), 'CODE.QUOTE', x, 'CODE.DO*RANGE'}}
      env.push('exec', recursion)
      env.push('exec', x)
    end
  end,

  ['define'] = function(env, stack)
    if env.depth('name') < 1 or env.depth(stack) < 1 then return end
    local name = env.pop('name')
    local code = env.pop(stack)
    env.bind_instruction(name, code)
  end,
}


-- reset the random seed once!
math.randomseed(os.time())

environment = {}
-- create a new program environment with empty stacks
function environment.new()
  local self = {}

  local stacks = {integer={}, float={}, boolean={}, code={}, exec={}, name={}}
  local defined_instructions = {}
  local names = {}
  local name_count = 0
  local eval_limit = 500
  local max_points = 50

  -- this isn't really a class method...
  function self.mutate(prog)
    -- sometimes generate a totally random program or return the same one
    local chance = math.random()
    if chance < 0.05 then
      return self.generate_random_program()
    end

    local exprs = parse_expr(prog)[1]
    --print("starting walk.")
    local length, text = get_program_length(exprs, true)
    --print(text)
    --print("program length: "..length)
    local target_pos = math.random(length)
    for v,group,index in walk_program(exprs) do
      if target_pos == 1 then
        -- actually mutate...
        if (length >= max_points) or (length > 1 and math.random() < 0.4) then
          --print('removing a node.')
          local remove_count = math.min(length-index, math.random(5))
          for i=1,remove_count do
            table.remove(group, index)
          end
          -- sometimes insert a new node in its place
          if math.random() < 0.4 then
            table.remove(group, index)
            for i=1,math.random(5) do
              table.insert(group, index, self.generate_random_term())
            end
          end
        else
          --print('inserting a node.')
          if math.random() < 0.8 then
            for i=1,math.random(5) do
              table.insert(group, index, self.generate_random_term())
            end
          else
            -- insert a grouped expression
            table.insert(group, index, self.generate_random_program(math.min(8, max_points - length)))
          end
        end
      end
      target_pos = target_pos - 1
    end
    length, text = get_program_length(exprs, true)
    -- hacky way to remove empty groups
    local empty_s,empty_e = text:find('%( %)')
    while empty_s do
      text = text:sub(1,empty_s-1)..text:sub(empty_e+1)
      empty_s,empty_e = text:find('%( %)')
    end

    -- but we might have deleted the whole program, so generate a new one
    if #text <= 2 then
      text = self.generate_random_program()
    end
    --print("new program: "..text)
    return text
  end

  function self.clear_names()
    names = {}
    name_count = 0
  end

  function self.add_name(name)
    names[name] = true
    name_count = name_count + 1
  end

  function self.generate_random_program(max_new_length)
    max_new_length = max_new_length or max_points
    -- actually only make tiny programs initially
    local prog = {}
    for i=1,math.random(math.min(6, max_new_length)) do
      prog[i] = self.generate_random_term()
    end
    return '('..table.concat(prog, ' ')..')'
  end

  function self.generate_random_term()
    local insn = nil
    if math.random() < 0.3 then
      insn = allowed_instructions[math.random(#allowed_instructions)]
    else
      local const_type = math.random()
      if const_type < 0.4 then
        -- random float [-1:+1]
        insn = math.random()*2 - 1
      elseif const_type < 0.8 then
        insn = math.random(100) - 50
      elseif const_type < 0.9 then
        if math.random() > 0.5 then
          insn = "true"
        else insn = "false"
        end
      else
        insn = self.get_possibly_random_name()
      end
    end
    return insn
  end

  function self.get_eval_limit()
    return eval_limit
  end

  function self.set_eval_limit(limit)
    eval_limit = limit
  end

  function self.set_max_points(points)
    max_points = points
  end

  function self.bind_instruction(name, code)
    defined_instructions[name] = code
  end

  function self.get_defined_instruction(name)
    return defined_instructions[name]
  end

  function self.get_possibly_random_name()
    local name = nil
    if math.random() > 0.2 then
      -- if there are existing names, prefer them by far
      local index = math.random(name_count)
      local i = 1
      for k,v in pairs(names) do
        if i == index then
          name = k
        end
        i = i + 1
      end
    end
    if name == nil then
      name = 'ainm_'..tostring(math.random(1024))
      names[name] = true
    end
    return name
  end

  function self.flush(stack_name)
    stacks[stack_name] = {}
  end

  function self.dup(stack_name)
    local stack = stacks[stack_name]
    if #stack > 0 then
      stack[#stack+1] = stack[#stack]
    end
  end

  function self.depth(stack_name)
    if not stacks[stack_name] then
      error("no stack? "..stack_name)
    end
    return #stacks[stack_name]
  end

  function self.push(stack_name, value)
    local stack = stacks[stack_name]
    if stack == nil then
      error("Trying to push ["..value.."] onto non-stack ["..stack_name.."]")
    end
    stack[#stack+1] = value
  end

  function self.pop(stack_name)
    local stack = stacks[stack_name]
    if stack == nil then
      error("Trying to pop from non-stack ["..stack_name.."]")
    end

    local value = stack[#stack]
    stack[#stack] = nil
    return value
  end

  return self
end

-- iterator function, walk the program's elements recursively
-- yield the node, its containing table and index within the table
-- for mutation etc.
local function program_walker(prog, serialise)
  local index = 0
  for k,v in ipairs(prog) do
    index = index + 1
    coroutine.yield(v, prog, index)
    if type(v) == 'table' then
      serialise('(')
      program_walker(v, serialise)
      serialise(')')
    else serialise(v) end
  end
end

function walk_program(prog, flat_print_table)
  local serialise
  if flat_print_table then
    serialise = function(s) flat_print_table[#flat_print_table+1] = s end
  else
    serialise = function() end  -- do nothing...
  end
  return coroutine.wrap(function () program_walker(prog, serialise) end)
end

function get_program_length(exprs, and_program_text)
  local serialised = nil
  if and_program_text then serialised = {} end
  local length = 0
  for v, group, index in walk_program(exprs, serialised) do
    length = length + 1
  end
  if serialised then 
    return length, '('..table.concat(serialised, ' ')..')'
  else
    return length
  end
end
--[[
  length = length or 0
  for k,v in ipairs(prog) do
    if length == position then
      if replacement then
        local chance = math.random()
        if chance < 0.25 then
          prog[k] = replacement
        elseif chance < 0.5 then
          table.insert(prog, k+1, replacement)
        else
          table.remove(prog, k)
        end
      end
      return
    end
    if type(v) == 'table' then
      length = walk_program(v, replacement, position, length + 1)
    else
      length = length + 1
    end
  end
  return length
end]]--

function extract_term(expr)
  local split = expr:find('[ )]')

  -- found a space or closing paren? return <bit before split, rest>
  if split then
    local before = expr:sub(1,split-1)
    -- save the closing paren
    if expr:sub(split,split) == ')' then
      split = split-1
    end
    local rest = expr:sub(split+1)
    return before, expr:sub(split+1)
  end
  -- else it was the last expr?
  return expr,nil
end

function parse_expr(expr)
  local exprs = {}
  local counter = 0
  while expr and expr:len() > 0 do
    -- skip whitespace
    while expr:sub(1,1) == ' ' do
      expr = expr:sub(2)
    end

    counter = counter + 1
    assert(counter < 100000, 'Aborted evaluation of '..expr..', infinite loop?')
    if expr:sub(1,1) == ')' then
      -- end of a subexpression, return exprs and remaining outer expr
      return exprs,expr:sub(2)
    end

    if expr:sub(1,1) == '(' then
      -- parse subexpr
      expr = expr:sub(2)
      exprs[#exprs+1],expr = parse_expr(expr)
    else
      -- extract a literal term
      term,expr = extract_term(expr)
      if term then exprs[#exprs+1] = term end
    end
  end
  return exprs
end

function eval_program(code, env)
  --print('Evaluating: '..code)
  -- create environment
  env = env or environment.new()

  local exprs = parse_expr(code)
  -- push program onto exec and code stacks
  env.push('exec', exprs)
  env.push('code', exprs)
  -- clear and regenerate the list of recorded names
  env.clear_names()
  local eval_limit = env.get_eval_limit()

  local eval_count = 0
  -- eval loop
  repeat
    eval_count = eval_count + 1
    if eval_count > eval_limit then
      --print("Program exceeded maximum evaluation limit, terminating.")
      return env
    end

    local p = env.pop('exec')

    -- messy.

    if p then
      local term
      if type(p) == 'table' and p[1] then
        term = p[1]
      else term = p
      end
      if type(term) == 'number' then
        -- argh!
        term = tostring(term)
      end
      if type(term) == 'string' then
        term = term:lower()

        -- handle number literals
        local i = tonumber(term)
        if i then
          if term:find('%.') then  -- float
            env.push('float', i)
          else
            env.push('integer', i)
          end
        elseif term == 'true' or term == 'false' then
          -- boolean literal
          env.push('boolean', term)
        else
          -- if it's an instruction literal, execute it
          local instruction = instructions[term]
          if instruction then
            instruction(env)
          else
            -- check for generic instruction (STACKNAME.INSTRUCTION)
            local split = term:find('%.')
            if split then
              local stack_name = term:sub(1, split-1)
              local instruction_name = term:sub(split+1)
              local instruction = instructions[instruction_name]
              if instruction then
                instruction(env, stack_name)
              end
            else
              -- name?
              local instruction = env.get_defined_instruction(term)
              if instruction then
                env.push('exec', instruction)
              else
                env.push('name', term)
                env.add_name(term)
              end
            end
          end
        end
      elseif type(term) == 'table' then
        -- explode and push p backwards onto EXEC
        for j=#term,1,-1 do
          env.push('exec', {term[j]})
        end
      end
    end
  until not p

  -- return the stacks after executing the program
  return env
end

-- select random numbers from 1..max without replacement
function make_rnd_generator(max)
  local seen = {}
  local remaining = max
  return function()
    assert(remaining > 0)
    while true do
      local num = math.random(max)
      if not seen[num] then
        seen[num] = true
        remaining = remaining - 1
        return num
      end
    end
  end
end

local euler = math.exp(1)
local function sigmoid(t)
  return 1/(1+math.pow(euler, -t))
end

function run(fit_fun, pop_size, evaluations)
  local pop = {}
  local env = environment.new()
  local tournament_size = 8

  for i=1,pop_size do
    pop[i] = env.generate_random_program(3)
  end

  local best_prog, best_fitness

  for i=1,evaluations/tournament_size do
    local rnd_generate = make_rnd_generator(pop_size)
    local fitness = {} -- (index, fitness)

    for j=1,tournament_size do
      local index = rnd_generate()
      local fit = fit_fun(pop[index])
      if fit <= 0 then
        fit = fit - 1 + sigmoid(#pop[index]) -- parsimony pressure...
      end
      if not best_prog or fit < best_fitness then
        print('#'..i..' Fitness of p'..index..':', fit)
        best_fitness = fit
        best_prog = pop[index]
        steps_since_new_best = 0
      end
      fitness[j] = { index, fit }
    end

    -- sort by ascending fitness (i.e. best programs first)
    table.sort(fitness, function(a,b) return a[2] < b[2] end)

    --print(fitness[1][2], fitness[2][2], fitness[3][2], fitness[4][2])
    --print(fitness[1][1], fitness[2][1], fitness[3][1], fitness[4][1])
    for j=1,tournament_size/2 do
      -- replace losers
      local source_index = fitness[j][1]
      local replace_index = fitness[#fitness - j + 1][1]
      pop[replace_index] = env.mutate(pop[source_index])
      --print('replacing '..replace_index..' with '..source_index)
    end
  end
  return best_prog, best_fitness
end
