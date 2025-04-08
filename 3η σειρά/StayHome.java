import java.io.*;
import java.util.*;

public class StayHome {
	private static int n, m;
	private static char[][] input;
	private static Cell[][] grid;

	private static class Cell {
		public int r, c;
		public Cell prev = null;
		public char move = ' ';
		public int timeCor = Integer.MAX_VALUE, timeSot = Integer.MAX_VALUE;
		public boolean corVis = false, sotVis = false, airport = false;

		public Cell(int r, int c) {
			this.r = r;
			this.c = c;
		}

		public Queue<Cell> neighbours(){
			Queue<Cell> neigh = new ArrayDeque<>();
			if (r != n -1 && input[r+1][c] != 'X')
				neigh.add(grid[r+1][c]);
            if ( c != 0 && input[r][c-1] != 'X')
                neigh.add(grid[r][c-1]);
            if ( c != m-1 && input[r][c+1] != 'X')
                neigh.add(grid[r][c+1]);
            if ( r != 0 && input[r-1][c] != 'X')
                neigh.add(grid[r-1][c]);
            return neigh;

		}
	}

	public static void main(String[] args) throws FileNotFoundException {
		Scanner s = new Scanner(new File(args[0]));
		List<String> lines = new ArrayList<String>();
		while (s.hasNextLine()) {
			lines.add(s.nextLine());
		}
		String[] lineIn = lines.toArray(new String[0]);
		n = lineIn.length;
		m = lineIn[0].length();

		input = new char[n][m];
		for(int i = 0; i < lineIn.length; i++)
            input[i] = lineIn[i].toCharArray();

        solve();
	}

	public static void solve() {
		grid = new Cell[n][m];
		Queue<Cell> corona = new ArrayDeque<>();
		Queue<Cell> sotiris = new ArrayDeque<>();
		Queue<Cell> airplanes = new ArrayDeque<>();
		Cell safe_square = null;
		boolean usedAirports = false;

		for (int i = 0; i < n; i++){
			for (int j = 0; j < m; j++){
				char c = input[i][j];
				grid[i][j] = new Cell(i, j);
				if (c == 'S') {
					grid[i][j].timeSot = 0;
					grid[i][j].sotVis = true;
					safe_square = grid[i][j];
					sotiris.add(grid[i][j]);
				}
				else if (c == 'W') {
					grid[i][j].timeCor = 0;
					corona.add(grid[i][j]);
				}
				else if (c == 'A') {
					grid[i][j].airport = true;
					airplanes.add(grid[i][j]);
				}
			}
		}

		boolean flag = false;
		int firstA = 0;
		int time = 0;
		while(!corona.isEmpty()) {
			Cell temp = corona.remove();
			int x = temp.r, y = temp.c;
			time = temp.timeCor;
			if(!temp.corVis) {
				temp.corVis = true;
				if (grid[x][y].airport && !usedAirports){
					usedAirports = true;
					firstA = time + 5;
				}
				if (usedAirports && !flag) {
					if (firstA < time || corona.isEmpty()) {
						flag = true;
						for (Cell use : airplanes) {
							if (use.timeCor == Integer.MAX_VALUE) {
								use.timeCor = firstA;
								corona.add(use);
							}
							else if (use.timeCor > firstA) {
								use.timeCor = firstA;
								use.corVis = false;
								corona.add(use);
							}
						}
					}
				}
				for (Cell neigh : temp.neighbours()) {
					if (neigh.timeCor == Integer.MAX_VALUE) {
						neigh.timeCor = time + 2;
						corona.add(neigh);
					}
					else if (neigh.timeCor > time + 2) {
						neigh.timeCor = time + 2;
						neigh.corVis = false;
						corona.add(neigh);
					}
				}
			}
		}

		int safe_time = 0;
		while(!sotiris.isEmpty()) {
			Cell temp = sotiris.remove();
			time = temp.timeSot;
			int x = temp.r, y = temp.c;
			if (input[x][y] == 'T') {
				safe_time = time;
				safe_square = temp;
				break;
			}
			for (Cell neigh : temp.neighbours()) {
				if (neigh.timeSot == Integer.MAX_VALUE && neigh.timeCor > time + 1){
					neigh.timeSot = time + 1;
					neigh.prev = temp;
					if (neigh.c > temp.c)
                        neigh.move = 'R';
                    else if (neigh.c < temp.c)
                        neigh.move = 'L';
                    else if (neigh.r < temp.r)
                        neigh.move = 'U';
                    else
                        neigh.move = 'D';
					sotiris.add(neigh);
				}
			}
		}

		if (safe_time == 0) 
			System.out.println("IMPOSSIBLE\n");
		else {
			System.out.println(safe_time);
			Deque<Character> moves = new ArrayDeque<>();
			while(safe_square.prev != null){
				moves.addFirst(safe_square.move);
				safe_square = safe_square.prev;
			}
			for (char c: moves)
				System.out.print(c);
			System.out.print('\n');
		}
	}
}