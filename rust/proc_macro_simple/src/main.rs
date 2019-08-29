use proc_macro_simple::*;

minimal!(foo, "foobar");

fn main() {
    foo();
    println!("Hello, world!");
}
