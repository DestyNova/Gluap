require 'lunatest'
require 'gluap'

function test_create_program()
  local prog = gluap.environment.new()
  assert_not_nil(prog)
  prog.push('code','*')
  assert_equal('*', prog.pop('code'))
end

function test_eval_literal()
  local result = gluap.eval_program('7')
  assert_equal(7, result.pop('integer'))
end

function test_extract_term()
  local expr = '1 2 3'
  local term,expr = gluap.extract_term(expr)
  assert_equal('1', term)
  assert_equal('2 3', expr)
end

function test_parse_simple_expr()
  local r = gluap.parse_expr('1 2 +')
  assert_equal('1', r[1])
  assert_equal('2', r[2])
  assert_equal('+', r[3])
  assert_equal(3, #r)
end

function test_parse_list_expr()
  local r = gluap.parse_expr('(1 2 +)')
  assert_equal('1', r[1][1])
  assert_equal('2', r[1][2])
  assert_equal('+', r[1][3])
  assert_equal(1, #r)
end

function test_parse_compound_expr()
  local r = gluap.parse_expr('1 ( (2) ) +')
  assert_equal('1', r[1])
  assert_equal('2', r[2][1][1])
  assert_equal('+', r[3])
  assert_equal(3, #r)
end

function test_evaluates_simple_program()
  local result = gluap.eval_program('( 1 2 + )')
  assert_equal(3, result.pop('integer'))
end

function test_evaluates_more_complex_program()
  local result = gluap.eval_program '( 5 1.23 INTEGER.+ (4) INTEGER.- 5.67 FLOAT.*)'
  assert_equal(1, result.pop('integer'))
  assert_equal(6.9741, result.pop('float'))
end

function test_evaluates_more_complex_program2()
  local result = gluap.eval_program '( 2 3 INTEGER.* 4.1 5.2 FLOAT.+ TRUE FALSE BOOLEAN.OR )'
  assert_equal(6, result.pop('integer'))
  assert_equal(9.3, result.pop('float'))
  assert_equal('true', result.pop('boolean'))
  assert_equal('2', result.pop('code')[1][1]) -- check that we put something => CODE
end

function test_evaluates_dup_program()
  local result = gluap.eval_program('(5 INTEGER.DUP + )')
  assert_equal(10, result.pop('integer'))
end

function test_evaluates_code_quote_do()
  local result = gluap.eval_program('(CODE.QUOTE ( INTEGER.DUP INTEGER.+ ) 5 CODE.DO )') 
  assert_equal(10, result.pop('integer'))
end

function test_evaluates_code_define()
  local result = gluap.eval_program('(DOUBLE CODE.QUOTE ( INTEGER.DUP INTEGER.+ ) 5 CODE.DEFINE DOUBLE)') 
  assert_equal(10, result.pop('integer'))
  result = gluap.eval_program('( CODE.QUOTE ( INTEGER.DUP INTEGER.+ ) DOUBLE CODE.DEFINE 5 DOUBLE)') 
  assert_equal(10, result.pop('integer'))
end

function test_evaluates_exec_define()
  local result = gluap.eval_program('( DOUBLE EXEC.DEFINE ( INTEGER.DUP INTEGER.+ ) 5 DOUBLE)') 
  assert_equal(10, result.pop('integer'))
end

function test_evaluates_factorial_example()
  local env = gluap.environment.new()
  env.push('integer', 5)
  local result = gluap.eval_program('( CODE.QUOTE ( INTEGER.POP 1 ) CODE.QUOTE ( CODE.DUP INTEGER.DUP 1 INTEGER.- CODE.DO INTEGER.* ) INTEGER.DUP 2 INTEGER.< CODE.IF )', env)
  assert_equal(120, result.pop('integer'))
end

function test_evaluates_factorial_example2()
  local env = gluap.environment.new()
  env.push('integer', 5)
  local result = gluap.eval_program('( 1 INTEGER.MAX CODE.QUOTE ((INTEGER.*)) 1 CODE.DO*RANGE)', env)
  assert_equal(120, result.pop('integer'))
end

function test_evaluates_factorial_example3()
  local env = gluap.environment.new()
  env.push('integer', 5)
  local result = gluap.eval_program('( 1 INTEGER.MAX 1 EXEC.DO*RANGE INTEGER.* )', env)
  assert_equal(120, result.pop('integer'))
end

function test_evaluates_float_power_example()
  local env = gluap.environment.new()
  env.push('integer', 7)
  env.push('float', 2.5)
  local result = gluap.eval_program('(ARG FLOAT.DEFINE EXEC.Y (ARG FLOAT.* 1 INTEGER.- INTEGER.DUP 0 INTEGER.>       EXEC.IF ( ) EXEC.POP ) )', env)
  assert_equal(610.3515625, result.pop('float'))
end

function test_evaluates_code_if2()
  local env = gluap.environment.new()
  local prog = '(INTEGER.= CODE.QUOTE FLOAT.* CODE.QUOTE FLOAT./ CODE.IF )'
  env.push('integer', 5)
  env.push('integer', 5)
  env.push('float', 3)
  env.push('float', 4)
  local result = gluap.eval_program(prog, env)
  assert_equal(12, result.pop('float'))

  env.push('integer', 5)
  env.push('integer', 7)
  env.push('float', 3)
  env.push('float', 4)
  result = gluap.eval_program(prog, env)
  assert_equal(0.75, result.pop('float'))
end

function test_evaluates_code_if3()
  local env = gluap.environment.new()
  local prog = '(INTEGER.= EXEC.IF FLOAT.* FLOAT./)'
  env.push('integer', 5)
  env.push('integer', 5)
  env.push('float', 3)
  env.push('float', 4)
  local result = gluap.eval_program(prog, env)
  assert_equal(12, result.pop('float'))

  env.push('integer', 5)
  env.push('integer', 7)
  env.push('float', 3)
  env.push('float', 4)
  result = gluap.eval_program(prog, env)
  assert_equal(0.75, result.pop('float'))
end

function test_generates_random_program()
  local env = gluap.environment.new()
  local prog = env.generate_random_program()
  env.push('integer', 5)
  env.push('float', 4)
  local result = gluap.eval_program(prog, env)
  print('int:',env.pop('integer'),'float:',env.pop('float'),'bool:',env.pop('boolean'))
end

function test_mutates_program()
  local env = gluap.environment.new()
  local prog = '( 5 1.23 INTEGER.+ (4) INTEGER.- 5.67 FLOAT.*)'
  print('before:',prog)
  --assert_equal(8, gluap.walk_program(gluap.parse_expr(prog)[1]))
  print('after:',env.mutate(prog))
end

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

  local best,fitness = gluap.run(double_fitness, 250, 5000)
  print('best program so far with fitness '..fitness,best)
end

function disabled_extract_term()
  local a,b = gluap.extract_term('( 1 2 3)')
  print('XYZ',a,b)
  a,b = gluap.extract_term('(1 2 3)')
  print('XYZ2',a,b)
end

lunatest.run()
