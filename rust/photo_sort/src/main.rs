/* TODO
 * - implement interactive mode
 * - implement undo script
 */

extern crate clap;
#[macro_use]
extern crate lazy_static;
extern crate regex;

use clap::{Arg, App};
use regex::Regex;
use std::fs;
use std::io;
use std::option;
use std::path;

fn main() {
    let path_arg = Arg::with_name("path")
        .required(false)
        .takes_value(true)
        .short("p")
        .long("path")
        .help("Run inside a given directory.");
    let dry_run_arg = Arg::with_name("dry-run")
        .required(false)
        .takes_value(false)
        .short("d")
        .long("dry-run")
        .help("Do not create directories or move files.");
    let yes_arg = Arg::with_name("yes")
        .required(false)
        .takes_value(false)
        .short("y")
        .long("yes")
        .help("Non interactive mode which assumes yes for all inputs.");
    let undo_arg = Arg::with_name("undo")
        .required(false)
        .takes_value(false)
        .short("u")
        .long("undo")
        .help("Create an undo.sh shell script to undo the changes.");
    let args = App::new("photo_sort")
        .version("0.1.0")
        .author("Iñaki Garay <igarai@gmail.com>")
        .about("A small utility to sort image files.")
        .arg(path_arg)
        .arg(dry_run_arg)
        .arg(yes_arg)
        .arg(undo_arg)
        .get_matches();

    let workdir;
    if args.is_present("path") {
        workdir = path::Path::new(args.value_of("path").unwrap());
    }
    else {
        workdir = path::Path::new("./");
    }

    let cb = | x: &std::fs::DirEntry | {
        process_entry(&args, &workdir, &x.path());
    };
    visit_dirs(workdir, &cb).unwrap();

    if !args.is_present("dry-run") && args.is_present("undo") {
        println!("Creating undo script.");
    }
}

fn process_entry(args: &clap::ArgMatches, workdir: &path::Path, file_path: &path::Path) {
    match destination(&file_path) {
        Some(destination) => {
            match create_directory(&args, &workdir, &destination) {
                Ok(_) => {
                    match move_file(&args, &file_path, &workdir, &destination) {
                        Ok(_) => {}
                        Err(reason) => {
                            println!("Error moving file {:?}, reason: {:?}", file_path, reason);
                        }
                    }
                },
                Err(reason) => {
                    println!("Unable to create directory {:?}, reason: {:?}", destination, reason);
                }
            }
        },
        None => {}
    }
}

fn visit_dirs(dir: &path::Path, cb: &Fn(&std::fs::DirEntry)) -> io::Result<()> {
    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb)?;
            } else {
                cb(&entry);
            }
        }
    }
    Ok(())
}

fn destination(file: &path::Path) -> option::Option<path::PathBuf> {
    match file.to_str() {
        Some(filename) => {
            match date(filename) {
                Some((year, month, day)) => {
                    let dir = format!("{}-{}-{}", year, month, day);
                    let mut dest = path::PathBuf::new();
                    dest.push(dir);
                    Some(dest)
                },
                None => None
            }
        },
        None => None
    }
}

fn date(filename: &str) -> Option<(&str, &str, &str)> {

    lazy_static! {
        static ref RE: Regex = Regex::new(r".*(20[01][0-9]).?([01][0-9]).?([0123][0-9]).*").unwrap();
    }

    if RE.is_match(filename) {
        match RE.captures(filename) {
            Some(captures) => {
                let year = captures.get(1).unwrap().as_str();
                let month = captures.get(2).unwrap().as_str();
                let day = captures.get(3).unwrap().as_str();
                let tuple = (year, month, day);
                Some(tuple)
            },
            None => None
        }
    }
    else {
        None
    }
}

fn create_directory(args: &clap::ArgMatches, base_path: &path::Path, directory: &path::Path) -> io::Result<()> {
    let mut full_path = path::PathBuf::new();
    full_path.push(base_path);
    full_path.push(directory);

    if !args.is_present("dry-run") {
        match fs::create_dir(full_path) {
            Ok(_) => {
                Ok(())
            },
            Err(reason) => {
                match reason.kind() {
                    io::ErrorKind::AlreadyExists => {
                        Ok(())
                    },
                    _ => {
                        println!("directory could not be created: {:?}", reason.kind());
                        Err(reason)
                    }
                }
            },
        }
    }
    else {
        Ok(())
    }
}

fn move_file(args: &clap::ArgMatches, from: &path::Path, base_dir: &path::Path, dest_dir: &path::Path) -> io::Result<()> {
    let mut to = path::PathBuf::new();
    to.push(base_dir);
    to.push(dest_dir);
    to.push(from.file_name().unwrap());
    println!("{:?} => {:?}", from, to);
    if !args.is_present("dry-run") {
        match fs::rename(from, to) {
            Ok(_) => {
                if args.is_present("undo") {
                    println!("Saving undo information.");
                }
                Ok(())
            },
            Err(reason) => {
                println!("file {:?} could not be renamed: {:?}", from, reason);
                Err(reason)
            },
        }
    }
    else {
        Ok(())
    }
}