#[macro_use]
extern crate rustler;

use protobuf::Message;
use rustler::{Binary, Encoder, Env, Error, Term};

mod api;
mod common;
mod core;
mod domain;

#[cfg(test)]
mod tests {
    pub mod deck;
    pub mod game_effects;
    pub mod helpers;
    pub mod points;
    pub mod resources;
    pub mod trading;
}

mod atoms {
    rustler_atoms! {
        atom ok;
        //atom error;
        //atom __true__ = "true";
        //atom __false__ = "false";
    }
}

rustler::rustler_export_nifs! {
    "Elixir.Core",
    [
        ("ping", 1, ping)
    ],
    None
}

fn ping<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let data: Binary = args[0].decode()?;

    let ping_request = api::ping::PingRequest::parse_from_bytes(&data).unwrap();

    let mut ping_reply = api::ping::PingReply::new();
    ping_reply.set_message(format!("Hello, {}", ping_request.get_name()));

    Ok((atoms::ok(), ping_reply.write_to_bytes().unwrap()).encode(env))
}
