package edu.colorado.csci3155.project1


class RuntimeError(msg: String) extends Exception {
    override def toString(): String = {
        s"Error: $msg"
    }
}


object Interpreter {


    type Environment = Map[String, Value]



    /*--
        TODO: Complete the evalExpr Function below.
        Please write refactored code and use ValueOps.plus, ValueOps.minus,..
        defined in Value.scala for evaluating the expressions.

//        If you encounter error, you should throw a RuntimeError exception defined above.
        Please do not use other exception types.
     */


    def evalExpr(e: Expr, env: Environment) : Value = e match {
        case Const(f) => NumValue(f)
        case Ident(str) =>
            if ( env.contains(str) )  { env(str) }
            else { throw new RuntimeError("Identifier not bound to value in enviornment") }

        case Plus(e1, e2) => ValueOps.plus(evalExpr(e1, env), evalExpr(e2,env))
        case Minus(e1, e2) => ValueOps.minus(evalExpr(e1, env), evalExpr(e2,env))
        case Mult(e1, e2) => ValueOps.mult(evalExpr(e1, env), evalExpr(e2,env))

        case Div(e1, e2) =>  {
            if(e2 == Const(0.0)) throw new RuntimeError("Division by Zero")
            else {
                ValueOps.div(evalExpr(e1, env), evalExpr(e2,env))
            }

        }

        case Geq(e1, e2) => ValueOps.geq(evalExpr(e1, env), evalExpr(e2,env))
        case Gt(e1, e2) => ValueOps.gt(evalExpr(e1, env), evalExpr(e2,env))
        case Eq(e1, e2) => ValueOps.eq(evalExpr(e1, env), evalExpr(e2,env))

        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(false) => BoolValue(false)
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new RuntimeError("Non-BoolValue")
                    }
                }
                case _ => throw new RuntimeError("Non-BoolValue")
            }
        }

        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new RuntimeError("Non-BoolValue")
                    }
                }
                case _ => throw new RuntimeError("Non-BoolValue")
            }
        }

        case Not(e1) => {
            val v = evalExpr(e1, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new RuntimeError("Non-BoolValue")
            }
        }

        case IfThenElse(e1, e2, e3) => {
            val v = evalExpr(e1, env)
            v match {
                case BoolValue(true) => evalExpr(e2,env)
                case BoolValue(false) => evalExpr(e3, env)
                case _ => throw new RuntimeError("Non-BoolValue")
            }
        }
    }

    /*--
    TODO: Implement a function evalVarDefine that given a identifier x,
    expression e and environment env,
       a) evaluates e under env: let result be v
       b) yields new environment that updates env with {x -> v }
     For your convenience the RuntimeError exception has been handled for you.
     */
    def evalVarDefine(x: String, e: Expr, env: Environment): Environment = {
        try {
            env + (x -> evalExpr(e,env))
        } catch {
            case _:RuntimeError => env
        }

    }

    /*-- TODO: Complete the evalCommand Function Below --*/
    // Function evalCommand
    // Evaluate a command under an environment.
    //  Returns the new environment as a result of executing the command.
    //  If the command is of the form Define(x, e), the environment is updated by evaluating
    //  e under the "old" environment and updating the old environment to now bind x to the result.
    // If the command is of the form Display(e), the environment returned is just the
    // same as the environment that was passed in as an argument.
    //
    def evalCommand( env: Environment, cmd: Cmd): Environment = {
        cmd match {
            case Define(x, e) => {
                try {
                    val v = evalExpr(e, env)
                    evalVarDefine(x, e, env)
                } catch {
                    case _:RuntimeError =>  env
                }
            }
            case Display(e) => {
                println(env)
                env
            }
        }
    }

    /*-- TODO: Implement evalProgram function below.
       Careful: Do not use for/while loops. Instead you should be using
       pattern matching on `prog` and then using lst foldLeft function.
       A tail recursive solution is also acceptable but please try to use pattern matching.
     */
    def evalProgram(prog: CalcProgram, env0: Environment = Map.empty): Environment = {
        prog match {
            case TopLevel(listOfCmds) =>
                listOfCmds.foldLeft (env0) ((env: Environment, cmd: Cmd) =>
                    evalCommand(env, cmd))
        }
    }
}
