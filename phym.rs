/******************************************************************************/
/* Authors: Josh Carter, Jenna Provazek, Jon Sleep                            */
/* Date: March 3rd, 2017                                                      */
/* Assignment 6                                                               */
/******************************************************************************/

/*----------------------------------------------------------------------------*/
/* needed libraries                                                           */
/*----------------------------------------------------------------------------*/

use std::collections::HashMap;
use std::process;
use std::fmt;

/*----------------------------------------------------------------------------*/
/* macros                                                                     */
/*----------------------------------------------------------------------------*/

// for exiting cleanly while also satisfying the type checker
macro_rules! fail {
    ($inp: expr) => {
        process::exit(fail($inp));
    }
}

/*----------------------------------------------------------------------------*/
/* types                                                                      */
/*----------------------------------------------------------------------------*/

type Env = HashMap<String, Value>;

/*----------------------------------------------------------------------------*/
/* expressions                                                                */
/*----------------------------------------------------------------------------*/

#[derive(Clone, Debug)]
pub enum ExprC {
    Num { n : i32 },
    Id { s : String },
    If { case : Box<ExprC>, succ : Box<ExprC>, fail : Box<ExprC> },
    Lam { params : Vec<String>, body : Box<ExprC> },
    Binop { op : String, left : Box<ExprC>, right : Box<ExprC> },
    App { app : Box<ExprC>, args : Vec<ExprC> }
}

/*----------------------------------------------------------------------------*/
/* values                                                                     */
/*----------------------------------------------------------------------------*/

#[derive(Clone, Debug)]
enum Value {
    Num { n : i32 },
    Bool { b : bool },
    Clos { params : Vec<String> , body : ExprC, clo_env : Env}
}

/*----------------------------------------------------------------------------*/
/* fail                                                                       */
/*----------------------------------------------------------------------------*/

/*
    takes a string, prints the error, then causes the thread to panic
*/

fn fail(strn : &str) -> i32 {
    let msg = format!("{} {}", "PHYM:", String::from(strn));
    println!("{}", msg);
    return 1;
}

/*----------------------------------------------------------------------------*/
/* build_env                                                                  */
/*----------------------------------------------------------------------------*/

/*
    takes an environment and lists of bindings and adds them to the environment
*/

fn build_env(env : Env, keys : Vec<String>, vals : Vec<Value>) -> Env {
    let mut up_env = env.clone();

    for iter in 0..keys.len() {
            let key = keys[iter].clone();
            let val = vals[iter].clone();
            up_env.insert(key, val);
    }

    return up_env;
}

/*----------------------------------------------------------------------------*/
/* lookup_env                                                                 */
/*----------------------------------------------------------------------------*/

/*
    takes a character and an environement and searches the environemt for that
    binding. Will throw error if it does not exist in the environment
*/

fn lookup_env(env : Env, key : String) -> Value {
    if !env.contains_key(&key) {
        fail!("environment does not contain key");
    }
    return env.get(&key).unwrap().clone();
}

/*----------------------------------------------------------------------------*/
/* add_binop_env                                                         */
/*----------------------------------------------------------------------------*/

/*
    takes an environment and adds a closure containing a binop with the
    given string reference
*/

fn add_binop_env(env : &mut Env, sym : &str) {

    let empty_env : Env = HashMap::new();
    let left = Box::new(ExprC::Id{ s : String::from("a") });
    let right = Box::new(ExprC::Id{ s : String::from("b") });
    let bin = ExprC::Binop{ op : sym.to_string(), left : left, right : right };
    let mut params : Vec<String> = Vec::new();
    params.push(String::from("a"));
    params.push(String::from("b"));
    let clos = Value::Clos { params : params, body : bin, clo_env : empty_env};

    env.insert(sym.to_string(), clos);
}


/*----------------------------------------------------------------------------*/
/* build_base_env                                                             */
/*----------------------------------------------------------------------------*/

/*
    creates the initial environment used by interp. Contains primitive function
    keys and boolean keys
*/

fn build_base_env() -> Env {
    let mut base_env : Env = HashMap::new();

    add_binop_env(&mut base_env, "+");
    add_binop_env(&mut base_env, "-");
    add_binop_env(&mut base_env, "*");
    add_binop_env(&mut base_env, "/");
    add_binop_env(&mut base_env, "<=");

    base_env.insert(String::from("true"), Value::Bool{ b: true });
    base_env.insert(String::from("false"), Value::Bool{ b: false });

    return base_env;
}

/*----------------------------------------------------------------------------*/
/* unwrap_bool                                                                */
/*----------------------------------------------------------------------------*/

/*
    helper function to catch invalid if cases and returns the actual boolean
    inside of a ExprC::Bool
*/

fn unwrap_bool(val : Value) -> bool {
    match val {
        Value::Bool { b } => b,
        _ => fail!("cannot evaluate non-boolean values in if statement")
    }
}

/*----------------------------------------------------------------------------*/
/* phym_plus                                                                  */
/*----------------------------------------------------------------------------*/

/*
    takes two values and attempts to add them together
*/

fn phym_plus(a : Value, b : Value) -> Value {
    match a {
        Value::Num { n } => {
            let n_a = n;
            match b {
                Value::Num { n } =>
                    Value::Num{ n : n_a + n },
                _ => fail!("cannot add non-numbers")
            }
        }
        _ => fail!("cannot add non-numbers")
    }
}

/*----------------------------------------------------------------------------*/
/* phym_sub                                                                   */
/*----------------------------------------------------------------------------*/

/*
    takes two values and attempts to subtract them
*/

fn phym_sub(a : Value, b : Value) -> Value {
    match a {
        Value::Num { n } => {
            let n_a = n;
            match b {
                Value::Num { n } =>
                    Value::Num{ n : n_a - n },
                _ => fail!("cannot subtract non-numbers")
            }
        }
        _ => fail!("cannot subctract non-numbers")
    }
}

/*----------------------------------------------------------------------------*/
/* phym_mult                                                                  */
/*----------------------------------------------------------------------------*/

/*
    takes two values and attempts to multiply them
*/

fn phym_mult(a : Value, b : Value) -> Value {
    match a {
        Value::Num { n } => {
            let n_a = n;
            match b {
                Value::Num { n } =>
                    Value::Num{ n : n_a * n },
                _ => fail!("cannot multiply non-numbers")
            }
        }
        _ => fail!("cannot multiply non-numbers")
    }
}

/*----------------------------------------------------------------------------*/
/* phym_leq                                                                   */
/*----------------------------------------------------------------------------*/

/*
    takes two values and determines if the first is less than or equal to the
    second
*/

fn phym_leq(a : Value, b : Value) -> Value {
    match a {
        Value::Num { n } => {
            let n_a = n;
            match b {
                Value::Num { n } =>
                    Value::Bool{ b : n_a <= n },
                _ => fail!("cannot compare non-numbers")
            }
        }
        _ => fail!("cannot compare non-numbers")
    }
}


/*----------------------------------------------------------------------------*/
/* phym_div                                                                   */
/*----------------------------------------------------------------------------*/

/*
    takes two values and attempts to divide them
*/

fn phym_div(a : Value, b : Value) -> Value {
    match a {
        Value::Num { n } => {
            let n_a = n;
            match b {
                Value::Num { n } => {
                    if n == 0 {
                        fail!("cannot divide by zero");
                    }
                    Value::Num{ n : n_a / n }
                },
                _ => fail!("cannot divide non-numbers")
            }
        }
        _ => fail!("cannot divide non-numbers")
    }
}

/*----------------------------------------------------------------------------*/
/* map_binop                                                                  */
/*----------------------------------------------------------------------------*/

/*
    takes a symbol from a binop and returns a closure to operate on the two
    given values
*/

fn interp_binop(op : String, a : Value, b : Value) -> Value {
    match op.as_ref() {
         "+" => phym_plus(a, b),
         "-" => phym_sub(a, b),
         "*" => phym_mult(a, b),
         "/" => phym_div(a, b),
         "<=" => phym_leq(a, b),
         _ => fail!("invalid binary operator")
    }
}

/*----------------------------------------------------------------------------*/
/* interp_list                                                                */
/*----------------------------------------------------------------------------*/

/*
    interps all the expressions in a given list with the same environement
*/

fn interp_list(exps : Vec<ExprC>, env : Env) -> Vec<Value> {

    let mut evals : Vec<Value> = Vec::new();

    for iter in 0..exps.len() {
        let item = exps[iter].clone();
        let eval = interp(item, env.clone());
        evals.push(eval);
    }

    return evals;
}

/*----------------------------------------------------------------------------*/
/* interp_app                                                                 */
/*----------------------------------------------------------------------------*/

/*
    attempts to interpret an application
*/

fn interp_app(app : ExprC, args : Vec<ExprC>, env : Env) -> Value {
    let clos = interp(app, env.clone());
    match clos {
        Value::Clos { params, body, clo_env } => {
            if (args.len() == params.len()) {
                let eval_args = interp_list(args, env.clone());
                let up_env = build_env(clo_env.clone(), params, eval_args);
                interp(body, up_env)
            }
            else {
                fail!("incorrect number of arguments");
            }
        },
        _ => fail!("not a valid application")
    }
}

/*----------------------------------------------------------------------------*/
/* interp                                                                     */
/*----------------------------------------------------------------------------*/

/*
    takes a PHYM ast and evaluates it
*/

fn interp(expr : ExprC, env : Env) -> Value {
    match expr {
        ExprC::Num { n } =>
            Value::Num { n : n },

        ExprC::Id { s } =>
            lookup_env(env, s),

        ExprC::If { case, succ, fail } =>
            if (unwrap_bool(interp(*case, env.clone()))) {
                interp(*succ, env)
            }
            else {
                interp(*fail, env)
            },

        ExprC::Lam { params, body } =>
            Value::Clos{ params : params, body : *body, clo_env : env.clone()},

        ExprC::Binop  { op, left, right } =>
            interp_binop(op, interp(*left, env.clone()),
                interp(*right, env.clone())),

        ExprC::App { app, args } =>
            interp_app(*app, args, env)
    }
}


/*----------------------------------------------------------------------------*/
/* serialize                                                                  */
/*----------------------------------------------------------------------------*/

/*
    takes a legal PHYM value and converts it to a string for logging
*/

fn serialize(val : Value) -> String {
    match val {
        Value::Num { n } =>
            n.to_string(),

        Value::Bool { b } =>
            if b { String::from("true") }
            else { String::from("false")},

        Value::Clos { params, body, clo_env} =>
            String::from("#procedure")
    }
}

/*----------------------------------------------------------------------------*/
/* main                                                                       */
/*----------------------------------------------------------------------------*/

fn main() {
    let base_env = build_base_env();
    let arg1 = ExprC::Num{ n : 10 };
    let arg2 = ExprC::Num{ n : 20 };
    let mut args : Vec<ExprC> = Vec::new();
    args.push(arg1);
    args.push(arg2);
    let id_box = Box::new(ExprC::Id{ s : String::from("+") });
    let test_app = ExprC::App{ app: id_box, args : args };


    println!("{}", serialize(interp(test_app, base_env)))
}
