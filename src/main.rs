#![feature(advanced_slice_patterns, slice_patterns)]
extern crate sexp;
mod phym;
use std::env;



/*----------------------------------------------------------------------------*/
/* main                                                                       */
/*----------------------------------------------------------------------------*/

fn main() {
    
    let args: Vec<_> = env::args().collect();
    if args.len() > 1 {
        println!("{:?}",phym::top_interp(args[1].as_str()))
    }
    else {
        let sexp = sexp::parse("(+ 1 2)").unwrap();
        println!("{:?}",phym::top_interp("(+ 1 2)"))
    }

    
}