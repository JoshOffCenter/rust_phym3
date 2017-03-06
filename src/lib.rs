//put tests in here
#![feature(advanced_slice_patterns, slice_patterns)]
mod phym;
extern crate sexp;

#[test]
fn test_add() {
    assert!(phym::top_interp("(+ 1 2)").as_str() == "3");
}

#[test]
fn true_if() {
    assert!(phym::top_interp("(if true 1 0)").as_str() == "1");
}

#[test]
fn false_if() {
    assert!(phym::top_interp("(if false 1 0)").as_str() == "0");
}
