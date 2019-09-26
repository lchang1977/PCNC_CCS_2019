//todo:
//  add ability to output to command line
//  allow sign and verify to work from command line arguments (no file loading)

extern crate base64;
extern crate clap;
extern crate regex;
extern crate ring;
extern crate untrusted;

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::process;

use clap::{App, Arg, ArgMatches, SubCommand};

use ring::{
    rand,
    signature::{self, KeyPair},
};

use regex::Regex;

fn main() {
    let matches = App::new("PCNC Crypto")
        .version("0.0.1")
        .author("John H. Ring IV")
        .about("Provides crypto functionality for the PCNC project")
        .subcommand(
            SubCommand::with_name("create")
                .about("Creates a ED25519 private key")
                .arg(
                    Arg::with_name("path")
                        .short("p")
                        .help("Location to write private key")
                        .required(true)
                        .long("path")
                        .value_name("PATH")
                        .takes_value(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("public")
                .about("Creates a public key from a ED25519 private key")
                .arg(
                    Arg::with_name("path")
                        .short("p")
                        .help("Location of existing private key")
                        .required(true)
                        .long("path")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("out")
                        .short("o")
                        .help("Location to write the public key")
                        .required(true)
                        .long("out")
                        .takes_value(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("sign")
                .about("Sign a message with an existing private key")
                .arg(
                    Arg::with_name("path")
                        .short("p")
                        .help("Location of existing private key")
                        .required(true)
                        .long("path")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("msg")
                        .short("m")
                        .help("Message to sign")
                        .required(true)
                        .long("message")
                        .takes_value(true),
                ),
        )
        .subcommand(
            SubCommand::with_name("verify")
                .about("Verify a signed message using a public key")
                .arg(
                    Arg::with_name("msg")
                        .short("m")
                        .help("Playing text message")
                        .required(true)
                        .long("message")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("sig")
                        .short("s")
                        .help("message signature")
                        .required(true)
                        .long("signature")
                        .takes_value(true),
                ),
        )
        .get_matches();
    if let Err(e) = run(matches) {
        println!("Application Error: {:?}", e);
        process::exit(1);
    }
}

fn run(matches: ArgMatches) -> Result<(), MyError> {
    match matches.subcommand() {
        ("create", Some(m)) => run_create(m),
        ("public", Some(m)) => run_public(m),
        ("sign", Some(m)) => run_sign(m),
        ("verify", Some(m)) => run_verify(m),
        _ => Ok(()),
    }
}

fn write_as_base64(bytes: &[u8], path: &str) -> Result<(), MyError> {
    let mut file = File::create(path).map_err(|e| MyError::IO(e))?;
    let bytes = base64::encode(bytes);
    file.write_all(bytes.as_ref()).map_err(|e| MyError::IO(e))?;
    Ok(())
}

fn run_create(matches: &ArgMatches) -> Result<(), MyError> {
    let path = matches.value_of("path").unwrap();
    let rng = rand::SystemRandom::new();

    let pkcs8_bytes =
        signature::Ed25519KeyPair::generate_pkcs8(&rng).map_err(|e| MyError::Unspecified(e))?;

    write_as_base64(pkcs8_bytes.as_ref(), path)
}

fn run_sign(matches: &ArgMatches) -> Result<(), MyError> {
    let key_path = matches.value_of("path").unwrap();
    let msg = matches.value_of("msg").unwrap().as_bytes();

    let key_pair = load_priv_key(key_path)?;
    let sig = key_pair.sign(msg);

    println!("{}", base64::encode(sig.as_ref()));
    Ok(())
}

fn load_priv_key(key: &str) -> Result<signature::Ed25519KeyPair, MyError> {
    let key = read_file(key)?;
    let key = untrusted::Input::from(&key);
    let key = signature::Ed25519KeyPair::from_pkcs8(key).map_err(|e| MyError::KeyRejected(e));
    return key;
}

fn run_public(matches: &ArgMatches) -> Result<(), MyError> {
    let key_path = matches.value_of("path").unwrap();
    let out_path = matches.value_of("out").unwrap();
    let key_pair = load_priv_key(key_path)?;
    let public_key_bytes = key_pair.public_key().as_ref();

    write_as_base64(public_key_bytes, out_path)
}

fn run_verify(matches: &ArgMatches) -> Result<(), MyError> {
    let re = Regex::new(r"\(Says\(Principal (.*?)\)").unwrap();

    let msg = matches.value_of("msg").unwrap();
    let public_key = re.captures(msg).unwrap();
    let public_key = public_key.get(1).map_or("", |m| m.as_str());
    let public_key = base64::decode(&public_key).map_err(|e| MyError::DecodeError(e))?;
    let public_key = untrusted::Input::from(&public_key);

    let sig = matches.value_of("sig").unwrap();
    let sig = base64::decode(&sig).map_err(|e| MyError::DecodeError(e))?;

    let msg = untrusted::Input::from(msg.as_bytes());
    let sig = untrusted::Input::from(&sig);
    let x = signature::verify(&signature::ED25519, public_key, msg, sig);
    let x = match x {
        Ok(_) => true,
        Err(_) => false,
    };
    println!("{}", x);
    Ok(())
}

#[derive(Debug)]
enum MyError {
    IO(std::io::Error),
    KeyRejected(ring::error::KeyRejected),
    Unspecified(ring::error::Unspecified),
    DecodeError(base64::DecodeError),
}

fn read_file(path: &str) -> Result<Vec<u8>, MyError> {
    let path = Path::new(path);

    let mut file = std::fs::File::open(path).map_err(|e| MyError::IO(e))?;

    let mut contents: Vec<u8> = Vec::new();
    file.read_to_end(&mut contents)
        .map_err(|e| MyError::IO(e))?;

    let contents = base64::decode(&contents).map_err(|e| MyError::DecodeError(e))?;
    Ok(contents)
}
