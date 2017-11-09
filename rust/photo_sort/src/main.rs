/* TODO
 * - move getting workdir into aux function
 * - handle io errors correctly in visit_dirs
 * - implement interactive mode
 * - implement undo script
 */

extern crate clap;
#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate walkdir;

use clap::{Arg, App};
use regex::Regex;
use std::fs;
use std::io;
use std::option;
use std::path::{Path,PathBuf};
use walkdir::WalkDir;

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
    let silent_arg = Arg::with_name("silent")
        .required(false)
        .takes_value(false)
        .short("s")
        .long("silent")
        .help("Silent running with no output.");
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
        .arg(silent_arg)
        .arg(undo_arg)
        .get_matches();

    let workdir: &Path;
    if args.is_present("path") {
        workdir = Path::new(args.value_of("path").unwrap());
    }
    else {
        workdir = Path::new("./");
    }

    for entry in WalkDir::new(workdir) {
        process_entry(&args, &workdir, &entry.unwrap().path())
    }

    if !args.is_present("dry-run") && args.is_present("undo") {
        if !args.is_present("silent") {
            println!("Creating undo script.");
        }
    }
}

fn process_entry(args: &clap::ArgMatches, workdir: &Path, entry: &Path) {
    if !entry.is_dir() {
        match destination(&workdir, &entry) {
            Some(destination) => {
                match create_directory(&args, &destination) {
                    Ok(_) => {
                        match move_file(&args, &entry, &destination) {
                            Ok(_) => {}
                            Err(reason) => {
                                if !args.is_present("silent") {
                                    println!("Error moving file {:?}, reason: {:?}", entry, reason);
                                }
                            }
                        }
                    },
                    Err(reason) => {
                        if !args.is_present("silent") {
                            println!("Unable to create directory {:?}, reason: {:?}", destination, reason);
                        }
                    }
                }
            },
            None => {}
        }
    }
}

fn destination(base_dir: &Path, file_name: &Path) -> option::Option<PathBuf> {
    file_name.to_str().and_then(date).map(|(year, month, day)| {
        base_dir.join(format!("{}-{}-{}", year, month, day))
    })
}

fn date(filename: &str) -> Option<(&str, &str, &str)> {

    lazy_static! {
        // YYYY?MM?DD or YYYYMMDD,
        // where YYYY in [2000-2019], MM in [01-12], DD in [01-31]
        static ref RE: Regex = Regex::new(r"\D*(20[01]\d).?(0[1-9]|1[012]).?(0[1-9]|[12]\d|30|31)\D*").unwrap();
    }

    RE.captures(filename).map(|captures| {
        let year = captures.get(1).unwrap().as_str();
        let month = captures.get(2).unwrap().as_str();
        let day = captures.get(3).unwrap().as_str();
        (year, month, day)
    })
}

fn create_directory(args: &clap::ArgMatches, directory: &Path) -> io::Result<()> {
    if !args.is_present("dry-run") {
        let mut full_path = PathBuf::new();
        full_path.push(directory);
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
                        if !args.is_present("silent") {
                            println!("Error: directory could not be created: {:?}", reason.kind());
                        }
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

fn move_file(args: &clap::ArgMatches, from: &Path, dest: &Path) -> io::Result<()> {
    let mut to = PathBuf::new();
    to.push(dest);
    to.push(from.file_name().unwrap());

    if !args.is_present("silent") {
        println!("{:?} => {:?}", from, to)
    }

    if !args.is_present("dry-run") {
        match fs::rename(from, to) {
            Ok(_) => {
                if args.is_present("undo") {
                    if !args.is_present("silent") {
                        println!("Saving undo information.");
                    }
                }
                Ok(())
            },
            Err(reason) => {
                if !args.is_present("silent") {
                    println!("Error: file {:?} could not be renamed: {:?}", from, reason);
                }
                Err(reason)
            },
        }
    }
    else {
        Ok(())
    }
}