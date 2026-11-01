use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn init_thread_dispatcher(bg_script: JsValue) -> JsValue /* Promise */ {
    wasm_bindgen_spawn::init_bg_web(bg_script, wasm_bindgen::module())
        .create_dispatcher_promise()
        .into()
}

#[wasm_bindgen]
pub fn lunar_init() {
    extern crate console_error_panic_hook;
    use std::panic;

    panic::set_hook(Box::new(console_error_panic_hook::hook));
}

#[wasm_bindgen]
pub fn enter() {
    use crate::uci::run_uci;

    run_uci();
}
