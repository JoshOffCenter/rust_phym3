/******************************************************************************/
/* Authors: Josh Carter, Jenna Provazek, Jon Sleep                            */
/* Date: March 3rd, 2017                                                      */
/* Assignment 6                                                               */
/******************************************************************************/
/*----------------------------------------------------------------------------*/
/* unstable features                                                            */
/*----------------------------------------------------------------------------*/
#![feature(advanced_slice_patterns, slice_patterns)]

/*----------------------------------------------------------------------------*/
/* external crates                                                            */
/*----------------------------------------------------------------------------*/
extern crate sexp;


/*----------------------------------------------------------------------------*/
/* needed libraries                                                           */
/*----------------------------------------------------------------------------*/
use std::collections::HashMap;
use std::collections::HashSet;
use std::process;
use std::fmt;
use std::result;
use sexp::Sexp;
use sexp::Atom;


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
        panic!("environment does not contain key");
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
    add_binop_env(&mut base_env, "eq?");

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
        _ => panic!("cannot evaluate non-boolean values in if statement")
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
                _ => panic!("cannot add non-numbers")
            }
        }
        _ => panic!("cannot add non-numbers")
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
                _ => panic!("cannot subtract non-numbers")
            }
        }
        _ => panic!("cannot subctract non-numbers")
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
                _ => panic!("cannot multiply non-numbers")
            }
        }
        _ => panic!("cannot multiply non-numbers")
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
                _ => panic!("cannot compare non-numbers")
            }
        }
        _ => panic!("cannot compare non-numbers")
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
                        panic!("cannot divide by zero");
                    }
                    Value::Num{ n : n_a / n }
                },
                _ => panic!("cannot divide non-numbers")
            }
        }
        _ => panic!("cannot divide non-numbers")
    }
}

fn phym_eq(x : Value, y : Value) -> Value {
    match x {
        Value::Num { n } => {
            let l = n;
            match y {
                Value::Num { n } => {
                   let r = n;
                   Value::Bool{ b : l == r }
                }
                _ => Value::Bool{ b : false }
            }
        },
        Value::Bool { b } => {
            let l = b;
            match y {
                Value::Bool { b } => {
                   let r = b;
                   Value::Bool{ b : l == r }
                },
                _ => Value::Bool{ b : false }
            }
        },
        _ => Value::Bool{ b : false }
        
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
         "eq?" => phym_eq(a,b),
         _ => panic!("invalid binary operator")
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
                panic!("incorrect number of arguments");
            }
        },
        _ => panic!("not a valid application")
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
/* isNum / toNum                                                                  */
/*----------------------------------------------------------------------------*/

/*
    helpers to check if a string can be translated to a number and does it
*/

fn isNum(numStr : &str) -> bool {
    let res = numStr.parse::<i32>();
    match res {
        Ok(val) => true,
        Err(E) => false
    }
}

fn toNum(numStr : &str) -> i32 {
    numStr.parse::<i32>().unwrap()
}


/*----------------------------------------------------------------------------*/
/* parse                                                                  */
/*----------------------------------------------------------------------------*/

/*
    takes a str and parses to ExprC
*/

fn parse(p : Sexp) -> ExprC {
    let ops : HashSet<&str> = ["+","-","*","/","eq?","<="].iter().cloned().collect();

    match p {
        Sexp::Atom(a) => {
            match a {
                Atom::I(n) => {ExprC::Num{n : n as i32}},
                Atom::S(s) => {ExprC::Id{s : s}},
                _ => fail!("unimplemented")
            }
        },
        Sexp::List(v) => {
            match &v[..] {
                &[Sexp::Atom(Atom::S(ref op)),ref left,ref right] if ops.contains(op.as_str()) => {
                    ExprC::Binop{op:op.clone(),
                    left: Box::new(parse(left.clone())),
                    right: Box::new(parse(right.clone()))}
                },
                &[Sexp::Atom(Atom::S(ref if_str)),ref case,ref succ,ref fail] if *if_str == String::from("if") => {
                    ExprC::If{
                        case: Box::new(parse(case.clone())),
                        succ: Box::new(parse(succ.clone())),
                        fail: Box::new(parse(fail.clone()))}
                },
                _ => fail!("couldn't match in parse")
            }
        },
        _ => fail!("couldn't match in parse")
    }  
}

pub fn top_interp(s : &str) -> String {
    let base_env = build_base_env();
    let sexp = sexp::parse(s).unwrap();
    serialize(interp(parse(sexp),base_env))
}

// Conditionally compile the module `test` only when the test-suite is run.
#[cfg(test)]
mod test {
    use super::*;

    /*----------------------------------------------------------------------------*/
    /* serialize tests                                                            */
    /*----------------------------------------------------------------------------*/

    #[test]
    fn serialize_num_tst() {
        assert_eq!(serialize (Value::Num { n : 10 }), "10");
    }

    #[test]
    fn serialize_bool_tst() {
        assert_eq!(serialize (Value::Bool { b : true}), "true");
    }

    #[test]
    fn serialize_clos_tst() {
        assert_eq!(serialize (Value::Clos {params : Vec::new(), body : ExprC::Num{n : 10}, clo_env : build_base_env()}), "#procedure");
    }

    /*----------------------------------------------------------------------------*/
    /* phym_binops tests                                                          */
    /*----------------------------------------------------------------------------*/

    #[test]
    fn phym_plus_tst() {
        assert_eq!(serialize(phym_plus (Value::Num {n : 2}, Value::Num {n : 4})), "6");
    }

    #[test]
    #[should_panic(expected = "cannot add non-numbers")]
    fn phym_plus_fail_tst() {
        assert_eq!(serialize(phym_plus (Value::Bool {b : true}, Value::Num {n : 4})), "5");
    }

    #[test]
    fn phym_sub_tst() {
        assert_eq!(serialize(phym_sub (Value:: Num {n : 5}, Value::Num {n : 3})), "2");
    }

    #[test]
    #[should_panic(expected = "cannot subtract non-numbers")]
    fn phym_sub_fail_tst() {
        assert_eq!(serialize(phym_sub (Value::Bool {b : true}, Value::Num {n : 4})), "5");
    }

     #[test]
    fn phym_mult_tst() {
        assert_eq!(serialize(phym_mult (Value::Num {n : 5}, Value::Num {n : 3})), "15");
    }

    #[test]
    #[should_panic(expected = "cannot multiply non-numbers")]
    fn phym_mult_fail_tst() {
        assert_eq!(serialize(phym_mult (Value::Num {n : 2}, Value::Bool {b : false})), "5");
    }

    #[test]
    fn phym_div_tst() {
        assert_eq!(serialize(phym_div (Value:: Num {n : 10}, Value::Num {n : 2})), "5");
    }

    #[test]
    #[should_panic(expected = "cannot divide non-numbers")]
    fn phym_div_fail_tst() {
        assert_eq!(serialize(phym_div (Value::Bool {b : true}, Value::Num {n : 4})), "5");
    }

    #[test]
    #[should_panic(expected = "cannot divide by zero")]
    fn phym_div_by_zero_tst() {
        assert_eq!(serialize(phym_div (Value::Num {n : 10}, Value::Num {n : 0})), "5");
    }

    #[test]
    fn phym_leq_tst() {
        assert_eq!(serialize(phym_leq (Value:: Num {n : 10}, Value::Num {n : 2})), "false");
    }

    #[test]
    #[should_panic(expected = "cannot compare non-numbers")]
    fn phym_leq_fail_tst() {
        assert_eq!(serialize(phym_leq (Value::Bool {b : true}, Value::Num {n : 4})), "5");
    }

    /*----------------------------------------------------------------------------*/
    /* unwrap_bool tests                                                          */
    /*----------------------------------------------------------------------------*/

    #[test]
    fn unwrap_bool_tst() {
        assert_eq!(unwrap_bool(Value::Bool {b : true}), true);
    }

    #[test]
    #[should_panic(expected = "cannot evaluate non-boolean values in if statement")]
    fn unwrap_bool_fail_tst() {
        assert!(unwrap_bool(Value::Num {n : 2}));
    }

    /*----------------------------------------------------------------------------*/
    /* interp_binop tests                                                         */
    /*----------------------------------------------------------------------------*/

    #[test]
    fn interp_binop_tst() {
        assert_eq!(serialize(interp_binop(String::from("+"), Value::Num{ n : 2}, Value::Num{n : 2})), "4");
    }

    #[test]
    #[should_panic(expected = "invalid binary operator")]
    fn interp_binop_fail_tst() {
        assert_eq!(serialize(interp_binop(String::from("&"), Value::Num{ n : 2}, Value::Num{n : 2})), "4");
    }

    /*----------------------------------------------------------------------------*/
    /* interp tests                                                               */
    /*----------------------------------------------------------------------------*/

    #[test]
    fn interp_tst_num() {
        assert_eq!(serialize (interp(ExprC::Num {n : 2}, build_base_env())), "2")
    }

    #[test]
    fn interp_tst_id() {
        let mut tst_env : Env = HashMap::new();
        tst_env.insert(String::from("a"), Value::Num{ n : 5 });

        assert_eq!(serialize(interp(ExprC::Id {s : String::from("a")}, tst_env)), "5")
    }

    #[test]
    fn interp_tst_if() {
        let case_box = Box::new(ExprC::Id{ s : String::from("true")});
        let succ_box = Box::new(ExprC::Num{n : 4});
        let fail_box = Box::new(ExprC::Num{n : 0});

        assert_eq!(serialize (interp(ExprC::If {case : case_box, succ : succ_box, fail : fail_box}, build_base_env())), "4");
    }

    #[test]
    fn interp_tst_lam() {
        let mut pars : Vec<String> = Vec::new();
        pars.push(String::from("val"));

        let lft = Box::new(ExprC::Num{n : 1});
        let rt = Box::new(ExprC::Num{n : 1});
        let b = Box::new(ExprC::Binop{op : String::from("+"), left : lft, right : rt});

        assert_eq!(serialize(interp(ExprC::Lam {params : pars, body : b}, build_base_env())), "#procedure");
    }

    #[test]
    fn interp_tst_binop() {
        let lft = Box::new(ExprC::Num{n : 2});
        let rt = Box::new(ExprC::Num{n : 4});

        assert_eq!(serialize(interp(ExprC::Binop {op : String::from("*"), left : lft, right : rt}, build_base_env())), "8");
    }

    #[test]
    fn interp_tst_app() {
        let mut pars : Vec<String> = Vec::new();
        pars.push(String::from("val"));

        let lft = Box::new(ExprC::Id{s : String::from("val")});
        let rt = Box::new(ExprC::Num{n : 1});
        let b = Box::new(ExprC::Binop{op : String::from("+"), left : lft, right : rt});

        let the_app = Box::new(ExprC::Lam {params : pars, body: b});

        let mut the_args : Vec<ExprC> = Vec::new();
        the_args.push(ExprC::Num{n : 5});

        assert_eq!(serialize(interp(ExprC::App {app : the_app, args : the_args}, build_base_env())), "6");
    }

    
    /*----------------------------------------------------------------------------*/
    /* interp_app tests                                                           */
    /*----------------------------------------------------------------------------*/
    
    #[test]
    #[should_panic(expected = "incorrect number of arguments")]
    fn interp_app_fail_tst_1 () {
        let mut pars : Vec<String> = Vec::new();
        pars.push(String::from("val"));

        let lft = Box::new(ExprC::Id{s : String::from("val")});
        let rt = Box::new(ExprC::Num{n : 1});
        let b = Box::new(ExprC::Binop{op : String::from("+"), left : lft, right : rt});

        let the_app = ExprC::Lam {params : pars, body: b};

        let mut the_args : Vec<ExprC> = Vec::new();
        the_args.push(ExprC::Num{n : 5});
        the_args.push(ExprC::Num{n : 2});

        assert_eq!(serialize(interp_app(the_app, the_args, build_base_env())), "6");
    }

    #[test]
    #[should_panic(expected = "not a valid application")]
    fn interp_app_fail_tst_2 () {
        let mut pars : Vec<String> = Vec::new();
        pars.push(String::from("val"));

        let the_app = ExprC::Num {n : 4};

        let mut the_args : Vec<ExprC> = Vec::new();
        the_args.push(ExprC::Num{n : 5});
        the_args.push(ExprC::Num{n : 2});

        assert_eq!(serialize(interp_app(the_app, the_args, build_base_env())), "6");
    }

    /*----------------------------------------------------------------------------*/
    /* lookup_env tests                                                           */
    /*----------------------------------------------------------------------------*/

    #[test]
    fn lookup_env_tst() {
        let mut tst_env : Env = HashMap::new();
        tst_env.insert(String::from("a"), Value::Bool{ b: true });
        assert_eq!((serialize(lookup_env(tst_env, String::from("a")))), "true");
    }

    #[test]
    #[should_panic(expected = "environment does not contain key")]
    fn lookup_env_fail_tst() {
        let mut tst_env : Env = HashMap::new();
        tst_env.insert(String::from("a"), Value::Bool{ b: true });
        assert_eq!((serialize(lookup_env(tst_env, String::from("b")))), "true");
    }

    /*----------------------------------------------------------------------------*/
    /* build_env tests                                                            */
    /*----------------------------------------------------------------------------*/

    fn build_env_help() -> Env {
        let mut keys : Vec<String> = Vec::new();
        keys.push(String::from("x"));
        keys.push(String::from("y"));

        let mut vals : Vec<Value> = Vec::new();
        vals.push(Value::Num{n : 1});
        vals.push(Value::Bool{b : false});

        return build_env(HashMap::new(), keys, vals);
    }

    #[test]
    fn build_env_tst1() {
        assert_eq!(serialize(lookup_env(build_env_help(), String::from("x"))), "1")
    }

    #[test]
    fn build_env_tst2() {
        assert_eq!(serialize(lookup_env(build_env_help(), String::from("y"))), "false")
    }

    /*----------------------------------------------------------------------------*/
    /* interp_list tests                                                          */
    /*----------------------------------------------------------------------------*/

    fn create_args() -> Vec<Value> {
        let mut exps : Vec<ExprC> = Vec::new();
        exps.push(ExprC::Id{ s : String::from("x")});
        exps.push(ExprC::Id{ s : String::from("y")});
        exps.push(ExprC::Num{ n : 8});

        let mut tst_env : Env = HashMap::new();
        tst_env.insert(String::from("x"), Value::Num{ n : 4 });
        tst_env.insert(String::from("y"), Value::Bool{ b : false });

        return interp_list(exps, tst_env);
    }

    #[test]
    fn interp_list_tst1() {
        let val0 = create_args()[0].clone();
        assert_eq!(serialize(val0), "4");
    } 

     #[test]
    fn interp_list_tst2() {
        let val1 = create_args()[1].clone();
        assert_eq!(serialize(val1), "false");
    } 

    #[test]
    fn interp_list_tst3() {
        let val2 = create_args()[2].clone();
        assert_eq!(serialize(val2), "8");
    } 
}

