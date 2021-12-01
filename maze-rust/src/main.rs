use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;
use std::fs::{read_to_string};

type Cell = (usize, usize);
type Maze = HashMap<Cell, char>;
type Path = Vec<Cell>;

fn main() -> Result<(), Box<dyn Error>> {
    let contents = read_to_string("maze.txt")?;
    let lines  = contents.lines().collect::<Vec<&str>>();
    let ans = match solve(&lines.as_slice()) {
        Some(path) => {
            let mut tmp = Vec::new();
            for (y, row) in lines.iter().enumerate() {
                let mut line = String::new();
                for (x, ch) in row.char_indices() {
                    let solved_char = if path.contains(&(x, y)) && ch == ' ' {
                        '$'
                    } else {
                        ch
                    };
                    line.push(solved_char);
                }
                tmp.push(line);
            }
            tmp.join("\n")
        },
        None => String::from("no answer")
    };
    println!("{}", ans);
    Ok(())
}

fn solve(lines: &[&str]) -> Option<Path> {
    let mz = parse_maze(lines);
    let start = mz.keys().find(|cell| mz.get(cell) == Some(&'S'))?;
    let goal = mz.keys().find(|cell| mz.get(cell) == Some(&'G'))?;
    by_b(&start, &goal, &mz)
}


fn next_steps(&(x, y): &Cell, mz: &Maze) -> Path {
    let nexts = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)];
    nexts.iter().filter(|&cell| mz.contains_key(cell) && mz.get(cell) != Some(&'*') ).cloned().collect()
}

fn parse_maze(lines: &[&str]) -> Maze {
    let mut mz: Maze = HashMap::new();
    for (y, line) in lines.iter().enumerate() {
        for (x, ch) in line.char_indices() {
            mz.insert((x, y), ch);
        }
    }
    mz
}

fn by_b(&start: &Cell, &goal: &Cell, mz: &Maze) -> Option<Path> {
    let mut queue = VecDeque::from(vec![vec![start]]) ;
    let mut moved : HashSet<Cell> = HashSet::new();
    moved.insert(start);
    loop {
        match queue.pop_front() {
            Some(path) if path[0] == goal  => return Some(path),
            Some(path) => for next_step in next_steps(&path[0], mz) {
                if !moved.contains(&next_step) {
                    let mut elem = path.clone();
                    elem.insert(0, next_step);
                    queue.push_back(elem);
                    moved.insert(next_step);
                }
            },
            None => return None
        }
    }
}
