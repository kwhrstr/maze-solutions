import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.*;
import java.util.stream.Collectors;

class Maze {
    private final List<String> lines;
    private final Map<Cell, Character> maze = new HashMap<>();
    private final Cell startCell;
    private final Cell goalCell;
    Maze (String filePath) throws IOException {
        lines = Files.readAllLines(new File(filePath).toPath());
        Cell sc = null;
        Cell gc = null;
        for (int y = 0; y < lines.size(); y++) {
            for (int x = 0; x < lines.get(y).length(); x++) {
                char c = lines.get(y).charAt(x);
                maze.put(new Cell(x,y), c);
                if (c == 'S' && sc == null) {
                    sc = new Cell(x, y);
                } else if (c == 'G' && gc == null) {
                    gc = new Cell(x, y);
                }
            }
        }
        startCell = sc;
        goalCell = gc;
    }


    private List<Cell> nextSteps(Cell cell) {
        final int x = cell.x;
        final int y = cell.y;
        List<Cell> nextCells = Arrays.asList(
                new Cell(x + 1, y),
                new Cell (x - 1, y),
                new Cell(x, y + 1),
                new Cell(x, y - 1)
        );
        return nextCells.stream()
                .filter(c -> maze.get(c) != '*')
                .collect(Collectors.toList());
    }

    void solve () {
        if (startCell == null || goalCell == null){
            System.out.println("start cell or goal cell is not exist");
            return;
        }

        Set<Cell> moved = new HashSet<>();
        Queue<List<Cell>> queue = new LinkedList<>();
        queue.add(Collections.singletonList(startCell));
        Set<Cell> solvedPath = null;
        while (!queue.isEmpty()){
            List<Cell> path = queue.poll();
            moved.addAll(path);
            if (path.isEmpty()) {
                continue;
            }
            List<Cell> nexts = nextSteps(path.get(path.size() - 1));
            for (Cell cell : nexts) {
                if (moved.contains(cell)) {
                    continue;
                }
                List<Cell> newPath = new ArrayList<>(path);
                newPath.add(cell);
                if (cell.equals(goalCell)) {
                    solvedPath = new LinkedHashSet<>(newPath);
                    break;
                }
                queue.add(newPath);
            }
            if (solvedPath != null){
                break;
            }
        }
        if (solvedPath == null) {
            System.out.println("not found the goal");
            return;
        }
        for (int y = 0; y < lines.size(); y++) {
            for (int x = 0; x < lines.get(y).length(); x++) {
                Cell cell = new Cell(x, y);
                char c = maze.get(cell);
                if (c != ' ') {
                    System.out.print(c);
                } else if (solvedPath.contains(cell)){
                    System.out.print('$');
                } else {
                    System.out.print(' ');
                }
            }
            System.out.println();
        }
    }

    static class Cell {
        final int x;
        final int y;
        Cell (int x, int y){
            this.x = x;
            this.y = y;
        }

        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof Cell)) {
                return false;
            }
            Cell cell = (Cell)obj;
            return x == cell.x && y == cell.y;
        }

        @Override
        public int hashCode() {
            return 17 * (x + y) + 31;
        }
    }

}