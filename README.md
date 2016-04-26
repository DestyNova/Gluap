###Gluap: A basic PushGP interpreter and genetic programming library

####Features and limitations
* Cut-down subset of the simplest PushGP instructions (see the allowed_instructions table in gluap.lua)
* Tournament selection
* Mutation operators only - crossover not implemented yet
* Tends to get stuck in local optima almost all the time!

####Usage
* See test cases in gluap-test.lua

####Example
```lua
function test_evolves_7x()
  -- test function: f(x) = 7x. Minimise sum of squared errors.
  local double_fitness = function(prog)
    local sum_error = 0
    for i=-10,10 do
      local env = gluap.environment.new()
      env.push('integer', i)
      gluap.eval_program(prog, env)
      local result = env.pop('integer')
      if result then
        sum_error = sum_error + math.pow(result - 7*i, 2)
      else
        -- punish for giving no result!
        return 10^10
      end
    end
    return sum_error
  end

  local best,fitness = gluap.run(double_fitness, 250, 10000)
  print('best program with fitness '..fitness,best)
end
```
