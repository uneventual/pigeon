#![feature(async_closure)]
use axum::{routing::get, Router};
use pigeon::pigeon;

#[tokio::main]
async fn main() {
    pigeon! { (let [app (.(Router::new) route "/" (get (fn (async "hello, world"))))
        listener (. (await (tokio::net::TcpListener::bind "0.0.0.0:3000")) unwrap)]
    (. (await (axum::serve listener app) ) unwrap)
    )}
    ()
}
