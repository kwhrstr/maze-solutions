import java.io.File;
import java.nio.file.Files;
import java.util.*;

public class Maze {
    private int start_x = 0;
    private int start_y = 0;
    private int goal_x = 0;
    private int goal_y = 0;
    private List<Block> openList = new ArrayList<>();

    public static void main(String[] args) throws Exception {
        new Maze().search();
    }

    /**
     * 探索実行 
     */
    private void search() throws Exception {
        //迷路テキスト読み込み  
        String MAZE_FILE = "Maze.txt";
        List<String> lineList = Files.readAllLines(new File(MAZE_FILE).toPath());
        final int length = lineList.size();

        Map<Cell, Block> mazeMap = new LinkedHashMap<>();
        for (int y = 0; y < lineList.size(); y++) {
            for(int x = 0; x < lineList.get(y).length(); x++) {
                String block = lineList.get(y).substring(x, x + 1);
                boolean start = false;
                boolean wall = false;
                boolean goal = false;
                switch (block) {
                    case " ":
                        break;
                    case "*":
                        wall = true;
                        break;
                    case "S":
                        start = true;
                        start_x = x;
                        start_y = y;
                        break;
                    case "G":
                        goal = true;
                        goal_x = x;
                        goal_y = y;
                        break;
                }
                mazeMap.put(new Cell(x, y), new Block(x, y, start, goal, wall));
            }
        }

        for (Map.Entry<Cell, Block> keyVal : mazeMap.entrySet()) {
            keyVal.getValue().expected =
                    Math.abs(goal_x - keyVal.getKey().x) + Math.abs(goal_y - keyVal.getKey().y);
        }

        Cell startCell = new Cell(start_x, start_y);
        Cell goalCell = new Cell(goal_x, goal_y);
        mazeMap.get(startCell).cost = 0;
        openList.add(mazeMap.get(startCell));

        //探索開始
        Set<Cell> stamped = new HashSet<>();
        while (true) {
            //スタートからゴールまでの期待値が最小のものから順次探索する  
            Block now = getShortestOpenBlock();
            stamped.add(new Cell(now.x, now.y));

            //現在地点がゴールに隣接していたら探索終了  
            final Cell nextRightCell = new Cell (now.x + 1, now.y);
            final Cell nextLeftCell = new Cell(now.x - 1, now.y);
            final Cell nextDownCell = new Cell(now.x, now.y + 1);
            final Cell nextUpCell = new Cell (now.x, now.y - 1);
            List<Cell> nexts = Arrays.asList(nextRightCell, nextDownCell, nextLeftCell, nextUpCell);
            if (nexts.contains(goalCell)) {
                mazeMap.get(goalCell).parent = now;
                break;
            }

            //現在地点の四方のうち、進めるブロックをオープンリストに追加
            if (now.x + 1 <= lineList.get(now.y).length() &&
                    !mazeMap.get(nextRightCell).wall &&
                    !stamped.contains(nextRightCell)) {
                mazeMap.get(nextRightCell).parent = now;
                mazeMap.get(nextLeftCell).cost = now.cost + 1;
                openList.add(mazeMap.get(nextRightCell));
            }
            if (now.x - 1 >= 0 &&
                    !mazeMap.get(nextLeftCell).wall &&
                    !stamped.contains(nextLeftCell)) {
                mazeMap.get(nextLeftCell).parent = now;
                mazeMap.get(nextLeftCell).cost = now.cost + 1;
                openList.add(mazeMap.get(nextLeftCell));
            }
            if (now.y + 1 <= length &&
                    !mazeMap.get(nextDownCell).wall &&
                    !stamped.contains(nextDownCell)) {
                mazeMap.get(nextDownCell).parent = now;
                mazeMap.get(nextDownCell).cost = now.cost + 1;
                openList.add(mazeMap.get(nextDownCell));
            }
            if (now.y - 1 >= 0 &&
                    !mazeMap.get(nextUpCell).wall &&
                    !stamped.contains(nextUpCell)) {
                mazeMap.get(nextUpCell).parent = now;
                mazeMap.get(nextUpCell).cost = now.cost + 1;
                openList.add(mazeMap.get(nextUpCell));
            }
        }

        //ゴールからスタートまでの道のりをマーキング  
        Block parent = mazeMap.get(goalCell).parent;
        while (true) {
            if (!parent.start) {
                parent.correct = true;
                parent = parent.parent;
            } else {
                break;
            }
        }

        //正解を出力
        for (int y = 0; y < length; y++) {
            for (int x = 0; x < lineList.get(y).length(); x++) {
                Block block = mazeMap.get(new Cell(x, y));
                if (block.wall) {
                    System.out.print("*");
                } else if (block.start) {
                    System.out.print("S");
                } else if (block.goal) {
                    System.out.print("G");
                } else if (block.correct) {
                    System.out.print("$");
                } else {
                    System.out.print(" ");
                }
            }
            System.out.println();
        }
    }

    /**
     * オープンされているブロックのうち、最小コストのブロックを返却する 
     * @return 最小コストのブロック
     */
    private Block getShortestOpenBlock() throws Exception {
        if (openList.isEmpty()) {
            throw new Exception("ゴールへ辿り着く道はありません。");
        }
        Block shortest = null;
        int index = 0;
        for (int i = 0; i < openList.size(); i++) {
            if (shortest == null ||
                    shortest.getPoint() > openList.get(i).getPoint()) {
                shortest = openList.get(i);
                index = i;
            }
        }
        return openList.remove(index);
    }

    public class Cell {
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

    /**
     * 迷路の１コマを表すインナークラス 
     */
    private class Block {
        //座標  
        final int x;
        final int y;
        //スタート地点か否か  
        final boolean start;
        //ゴール地点か否か  
        final boolean goal;
        //壁か否か  
        final boolean wall;
        //正解の軌道か否か  
        boolean correct = false;
        //スタート地点からの移動距離
        int cost;
        //ゴールまでの最短移動距離  
        int expected;
        //スタートからゴールまでの最小期待値  

        Block(int x, int y, boolean start, boolean goal, boolean wall) {
            this.x = x;
            this.y = y;
            this.start = start;
            this.goal = goal;
            this.wall = wall;
        }

        int getPoint() {
            return cost + expected;
        }
        //親ブロック  
        Block parent = null;
    }
}  