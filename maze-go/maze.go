package main

import (
	fmt "fmt"
	"io/ioutil"
	"strings"
)

type Cell struct {
	x int
	y int
}

type Maze map[Cell] rune

type Path []Cell


type MazeSet struct {
	maze Maze
	start Cell
	goal Cell
}

func nillCell() Cell {return Cell{- 1, - 1}}

func main() {
	bytes, err := ioutil.ReadFile("maze.txt")
	if err != nil {
		panic(err)
	}
	solve(strings.Split(string(bytes), "\n"))
}

func solve (lines []string) {
	mazeSet := parseMaze(lines)
	if mazeSet.start == nillCell() {
		fmt.Printf("there is not start point")
		return
	}
	if mazeSet.goal == nillCell() {
		fmt.Printf("there is not goal point")
		return
	}

	path := _byb(mazeSet.start, mazeSet.goal, mazeSet.maze)
	pathSet := map[Cell]struct{}{}
	for _ ,c := range path {
		pathSet[c] = struct{}{}
	}

	for y , line := range lines {
		solveLine := ""
		for x, ch := range line {
			_, ok := pathSet[Cell{x, y}]
			if mazeSet.maze[Cell{x, y}] == ' ' && ok {
				solveLine += string('$')
			} else {
				solveLine += string(ch)
			}
		}
		fmt.Println(solveLine)
	}
}


func nextSteps(cell Cell, maze Maze) []Cell {
	allNexts := [4]Cell{
		{cell.x + 1, cell.y},
		{cell.x - 1, cell.y},
		{cell.x, cell.y + 1},
		{cell.x, cell.y - 1},
	}
	var nexts []Cell
	for  _, next := range allNexts {
		if val, ok := maze[next]; ok && val != '*' {
			nexts = append(nexts, next)
		}
	}
	return nexts
}


func parseMaze (lines []string) MazeSet {
	parsed := MazeSet{
		map[Cell]rune{},
		nillCell(),
		nillCell()}
	for y, line := range lines {
		for x, ch := range line {
			parsed.maze[Cell{x, y}] = ch
			if ch == 'S'{
				parsed.start = Cell{x, y}
			}
			if ch == 'G' {
				parsed.goal = Cell{x, y}
			}
		}
	}
	return parsed
}

func _byb (startCell Cell, goalCell Cell, maze Maze) Path {
	queue := []Path {{startCell}}
	moved := map[Cell]struct{}{}
	moved[startCell] = struct{}{}
	for len(queue) > 0 {
		path := queue[0]
		if path[0] == goalCell {
			return path
		}
		queue = queue[1:]
		for _, next := range nextSteps(path[0], maze) {
			if _, m := moved[next]; m {continue}
			queue = append(queue, append(Path{next}, path...))
			moved[next] = struct{}{}
		}
	}
	return Path{}
}


