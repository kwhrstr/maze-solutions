package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

type Cell struct {
	x int
	y int
}

type Maze map[Cell]rune

type Path []Cell

type MazeStruct struct {
	maze  Maze
	start Cell
	goal  Cell
}

func nillCell() Cell { return Cell{-1, -1} }

func main() {
	bytes, err := ioutil.ReadFile("maze.txt")
	if err != nil {
		panic(err)
	}
	solve(strings.Split(string(bytes), "\n"))
}

/*
解答
いろいろな関数を組み合わせるだけ
startやgoalがなかったらメッセージ出力
解答が得られなければメッセージ出力
*/
func solve(lines []string) {
	mazeSet := parseMaze(lines)
	if mazeSet.start == nillCell() {
		fmt.Println("there is not start point")
		return
	}
	if mazeSet.goal == nillCell() {
		fmt.Println("there is not goal point")
		return
	}

	path, ok := _byb(mazeSet.start, mazeSet.goal, mazeSet.maze)
	if !ok {
		fmt.Println("there is no solution!!")
		return
	}
	pathSet := map[Cell]struct{}{}
	for _, c := range path {
		pathSet[c] = struct{}{}
	}

	for y, line := range lines {
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

/*
次の道を探す
壁や登録されていない道順は使わない
*/
func nextSteps(cell Cell, maze Maze) []Cell {
	allNexts := [4]Cell{
		{cell.x + 1, cell.y},
		{cell.x - 1, cell.y},
		{cell.x, cell.y + 1},
		{cell.x, cell.y - 1},
	}
	var nexts []Cell
	for _, next := range allNexts {
		if val, ok := maze[next]; ok && val != '*' {
			nexts = append(nexts, next)
		}
	}
	return nexts
}

func parseMaze(lines []string) MazeStruct {
	parsed := MazeStruct{
		map[Cell]rune{},
		nillCell(),
		nillCell()}
	for y, line := range lines {
		for x, ch := range line {
			parsed.maze[Cell{x, y}] = ch
			if ch == 'S' {
				parsed.start = Cell{x, y}
			}
			if ch == 'G' {
				parsed.goal = Cell{x, y}
			}
		}
	}
	return parsed
}

/*
幅優先探索
Queueでとりうる道順を取り出す
道順が'G'にたどり着いてたらclear
たどり着いていなかったらとりうる値を登録
更新した道順はQueueに登録
*/
func _byb(startCell Cell, goalCell Cell, maze Maze) (Path, bool) {
	queue := []Path{{startCell}}
	moved := map[Cell]struct{}{}
	moved[startCell] = struct{}{}
	for len(queue) > 0 {
		path := queue[0]
		if path[0] == goalCell {
			return path, true
		}
		queue = queue[1:]
		for _, next := range nextSteps(path[0], maze) {
			if _, m := moved[next]; m {
				continue
			}
			queue = append(queue, append(Path{next}, path...))
			moved[next] = struct{}{}
		}
	}
	return Path{}, false
}
