#include <bits/stdc++.h>
using namespace std;

#define RMAX 100
#define CMAX 100

class MineSweeper {
	public:
		int row, col, mine;
		int dirx[9] = { -1, -1, -1, 0, 0, 0, 1, 1, 1 };
		int diry[9] = { -1, 0, 1, -1, 0, 1, -1, 0, 1 };
		int mineGrid[RMAX][CMAX];
		
		void mineGenerator(int mineGrid[CMAX][CMAX], int row, int col, int mine)
		{
		    srand(time(NULL));
		    int seed;
		    int mines[row][col];
		 
		    for (int i = 0; i < row; i++) 
		    {
		        for (int j = 0; j < col; j++) 
		        {
		            seed = rand() % 100;
		            
		            if (seed < mine)
		                mines[i][j] = true;
		            else
		                mines[i][j] = false;
		        }
		    }
		 
		    cout << "User Input:\n";

		    for (int i = 0; i < row; i++) 
		    {
		        for (int j = 0; j < col; j++) 
		        {
		            mineGrid[i][j] = 0;

		            for (int k = 0; k < 9; k++) 
		            {
		                if (isValid(i + dirx[k], j + diry[k]) && (mines[i + dirx[k]][j + diry[k]]))
		                    mineGrid[i][j]++;
		            }
		 
		            cout << mineGrid[i][j] << " ";
		        }
		        cout << endl;
		    }
		}
		
		bool isValid(int x, int y)
		{
		    return (x >= 0 && y >= 0 && x < row && y < col);
		}
		
		void printGrid(bool grid[CMAX][RMAX])
		{
		    for (int i = 0; i < row; i++) 
		    {
		        for (int j = 0; j < col; j++) 
		        {
		            if (grid[i][j])
		                cout << " *";
		            else
		                cout << " _";
		        }
		        cout << "\n";
		    }
		}
		
		bool isSafe(int mineGrid[CMAX][RMAX], int x, int y)
		{
		    if (!isValid(x, y))	
		    	return false;
		 
		    for (int i = 0; i < 9; i++) 
		    {
		        if (isValid(x + dirx[i], y + diry[i]) && (mineGrid[x + dirx[i]][y + diry[i]] - 1 < 0))
		            return false;
		    }
		 
		    for (int i = 0; i < 9; i++) 
		    {
		        if (isValid(x + dirx[i], y + diry[i]))
		            mineGrid[x + dirx[i]][y + diry[i]]--;
		    }
		 
		    return true;
		}
		
		bool findUnvisited(bool visited[CMAX][RMAX], int& x, int& y)
		{
		    for (x = 0; x < row; x++)
		    {
		    	for (y = 0; y < col; y++)
		    	{
		    		if (!visited[x][y])
		                return true;
		    	}
		    }
		    return false;
		}
		
		bool isDone(int mineGrid[CMAX][RMAX], bool visited[CMAX][RMAX])
		{
		    bool done = true;
		    for (int i = 0; i < row; i++) 
		    {
		        for (int j = 0; j < col; j++) 
		        {
		            done = done && (mineGrid[i][j] == 0) && visited[i][j];
		        }
		    }
		 
		    return done;
		}
		
		bool mineSolverUtil(bool grid[CMAX][RMAX], int mineGrid[CMAX][RMAX], bool visited[CMAX][RMAX])
		{
		    if (isDone(mineGrid, visited))
		    	return true;

		    int x, y;
		 
		    if (!findUnvisited(visited, x, y))
		        return false;
		 
		    visited[x][y] = true;
		 
		    if (isSafe(mineGrid, x, y)) 
		    {
		        grid[x][y] = true;
		        if (mineSolverUtil(grid, mineGrid, visited))
		            return true;
		        grid[x][y] = false;
		        for (int i = 0; i < 9; i++) 
		        {
		            if (isValid(x + dirx[i], y + diry[i]))
		                mineGrid[x + dirx[i]][y + diry[i]]++;
		        }
		    }
		 
		    if (mineSolverUtil(grid, mineGrid, visited))
		        return true;

		    visited[x][y] = false;
		 
		    return false;
		}
		
		void mineSolver(int mineGrid[CMAX][CMAX], int row, int col)
		{
		    bool grid[CMAX][RMAX];
		    bool visited[CMAX][RMAX];
		    memset(grid, false, sizeof(grid));
		    memset(visited, false, sizeof(visited));

		    if (mineSolverUtil(grid, mineGrid, visited)) 
		        printGrid(grid);
		    else
		        printf("No solution exists\n");
		}
};
 
// Driver Code
int main()
{
    MineSweeper m;
    
	// User input
    cout << "Enter rows (max. 100): ";
    cin >> m.row;
    cout << "Enter columns (max. 100): ";
    cin >> m.col;
    cout << "Enter mines probability (max. 100): ";
    cin >> m.mine;
    
    m.mineGenerator(m.mineGrid, m.row, m.col, m.mine);
    m.mineSolver(m.mineGrid, m.row, m.col);
 
    return 0;
}