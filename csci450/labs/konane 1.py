### File: konane.py
### Classes defined: KonaneError, Konane, Player, SimplePlayer,
### RandomPlayer, HumanPlayer

import random
import copy

class KonaneError(AttributeError):
    """
    This class is used to indicate a problem in the konane game.
    """

class Konane:
    """
    This class implements Konane, the Hawaiian version of checkers.
    The board is represented as a two-dimensional list.  Each 
    location on the board contains one of the following symbols:
       'B' for a black piece
       'W' for a white piece
       '.' for an empty location
    The black player always goes first.  The opening moves by both
    players are special cases and involve removing one piece from
    specific designated locations.  Subsequently, each move is a
    jump over one of the opponent's pieces into an empty location.
    The jump may continue in the same direction, if appropriate.
    The jumped pieces are removed, and then it is the opponent's
    turn.  Play continues until one player has no possible moves,
    making the other player the winner. 
    """
    def __init__(self, n):
        self.size = n
        self.reset()
        
    def reset(self):
        """
        Resets the starting board state.
        """
        self.board = []
        value = 'B'
        for i in range(self.size):
            row = []
            for j in range(self.size):
                row.append(value)
                value = self.opponent(value)
            self.board.append(row)
            if self.size%2 == 0:
                value = self.opponent(value)

    def __str__(self):
        return self.boardToStr(self.board)

    def boardToStr(self, board):
        """
        Returns a string representation of the konane board.
        """
        result = "  "
        for i in range(self.size):
            result += str(i) + " "
        result += "\n"
        for i in range(self.size):
            result += str(i) + " "
            for j in range(self.size):
                result += str(board[i][j]) + " "
            result += "\n"
        return result

    def valid(self, row, col):
        """
        Returns true if the given row and col represent a valid location on
        the konane board.
        """
        return row >= 0 and col >= 0 and row < self.size and col < self.size

    def contains(self, board, row, col, symbol):
        """
        Returns true if the given row and col represent a valid location on
        the konane board and that lcoation contains the given symbol.
        """
        return self.valid(row,col) and board[row][col]==symbol

    def countSymbol(self, board, symbol):
        """
        Returns the number of instances of the symbol on the board.
        """
        count = 0
        for r in range(self.size):
            for c in range(self.size):
                if board[r][c] == symbol:
                    count += 1
        return count

    def opponent(self, player):
        """
        Given a player symbol, returns the opponent's symbol, 'B' for black,
        or 'W' for white.
        """
        if player == 'B':
            return 'W'
        else:
            return 'B'

    def distance(self, r1, c1, r2, c2):
        """
        Returns the distance between two points in a vertical or
        horizontal line on the konane board.
        """
        return abs(r1-r2 + c1-c2)

    def makeMove(self, player, move):
        """
        Updates the current board with the next board created by the given
        move.
        """
        self.board = self.nextBoard(self.board, player, move)

    def nextBoard(self, board, player, move):
        """
        Given a move for a particular player from (r1,c1) to (r2,c2) this
        executes the move on a copy of the current konane board.  It will
        raise a KonaneError if the move is invalid. It returns the copy of
        the board, and does not change the given board.
        """
        r1 = move[0]
        c1 = move[1]
        r2 = move[2]
        c2 = move[3]
        next = copy.deepcopy(board)
        if not (self.valid(r1, c1) and self.valid(r2, c2)):
            raise KonaneError
        if next[r1][c1] != player:
            raise KonaneError
        dist = self.distance(r1, c1, r2, c2)
        if dist == 0:
            if self.openingMove(board):
                next[r1][c1] = "."
                return next
            raise KonaneError
        if next[r2][c2] != ".":
            raise KonaneError
        jumps = dist//2
        dr = (r2 - r1)//dist
        dc = (c2 - c1)//dist
        for i in range(jumps):
            if next[r1+dr][c1+dc] != self.opponent(player):
                raise KonaneError
            next[r1][c1] = "."
            next[r1+dr][c1+dc] = "."
            r1 += 2*dr
            c1 += 2*dc
            next[r1][c1] = player
        return next

    def openingMove(self, board):
        """
        Based on the number of blanks present on the konane board, determines
        whether the current move is the first or second of the game.
        """
        return self.countSymbol(board, ".") <= 1

    def generateFirstMoves(self, board):
        """
        Returns the special cases for the first move of the game.
        """
        moves = []
        moves.append([0]*4)
        moves.append([self.size-1]*4)
        moves.append([self.size/2]*4)
        moves.append([(self.size/2)-1]*4)
        return moves

    def generateSecondMoves(self, board):
        """
        Returns the special cases for the second move of the game, based
        on where the first move occurred.
        """
        moves = []
        if board[0][0] == ".":
            moves.append([0,1]*2)
            moves.append([1,0]*2)
            return moves
        elif board[self.size-1][self.size-1] == ".":
            moves.append([self.size-1,self.size-2]*2)
            moves.append([self.size-2,self.size-1]*2)
            return moves
        elif board[self.size/2-1][self.size/2-1] == ".":
            pos = self.size/2 -1
        else:
            pos = self.size/2
        moves.append([pos,pos-1]*2)
        moves.append([pos+1,pos]*2)
        moves.append([pos,pos+1]*2)
        moves.append([pos-1,pos]*2)
        return moves

    def check(self, board, r, c, rd, cd, factor, opponent):
        """
        Checks whether a jump is possible starting at (r,c) and going in the
        direction determined by the row delta, rd, and the column delta, cd.
        The factor is used to recursively check for multiple jumps in the same
        direction.  Returns all possible jumps in the given direction.
        """
        if self.contains(board,r+factor*rd,c+factor*cd,opponent) and \
           self.contains(board,r+(factor+1)*rd,c+(factor+1)*cd,'.'):
            return [[r,c,r+(factor+1)*rd,c+(factor+1)*cd]] + \
                   self.check(board,r,c,rd,cd,factor+2,opponent)
        else:
            return []

    def generateMoves(self, board, player):
        """
        Generates and returns all legal moves for the given player using the
        current board configuration.
        """
        if self.openingMove(board):
            if player=='B':
                return self.generateFirstMoves(board)
            else:
                return self.generateSecondMoves(board)
        else:
            moves = []
            rd = [-1,0,1,0]
            cd = [0,1,0,-1]
            for r in range(self.size):
                for c in range(self.size):
                    if board[r][c] == player:
                        for i in range(len(rd)):
                            moves += self.check(board,r,c,rd[i],cd[i],1,
                                                self.opponent(player))
            return moves

    def playOneGame(self, p1, p2, show):
        """
        Given two instances of players, will play out a game
        between them.  Returns 'B' if black wins, or 'W' if
        white wins. When show is true, it will display each move
        in the game.
        """
        self.reset()
        p1.initialize('B')
        p2.initialize('W')
        while 1:
            if show:
                print(self)
                print("player B's turn")
            move = p1.getMove(self.board)
            if move == []:
                result = 'W'
                break
            self.makeMove('B', move)
            if show:
                print(move)
                print()
                print(self)
                print("player W's turn")
            move = p2.getMove(self.board)
            if move == []:
                result = 'B'
                break
            self.makeMove('W', move)
            if show:
                print(move)
                print()
        if show:
            print("Game over")
        return result

    def playNGames(self, n, p1, p2, show):
        """
        Will play out n games between player p1 and player p2.
        The players alternate going first.  Prints the total
        number of games won by each player.
        """
        p1.reset()
        p2.reset()
        first = p1
        second = p2
        for i in range(n):
            print("Game", i)
            winner = self.playOneGame(first, second, show)
            if winner == 'B':
                first.won()
                print(first.name, "wins")
            else:
                second.won()
                print(second.name, "wins")
            temp = first
            first = second
            second = temp
        print(first.name, first.wins, second.name, second.wins)
            

class Player:
    """
    A base class for Konane players.  All players must implement
    the the initialize and getMove methods.
    """
    name = "Player"
    wins = 0
    def won(self):
        self.wins += 1
    def reset(self):
        self.wins = 0
    def initialize(self, side):
        """
        Records the player's side, either 'B' for black or
        'W' for white.  Should also set the name of the player.
        """
        abstract()
    def getMove(self, board):
        """
        Given the current board, should return a valid move.
        """
        abstract()


class SimplePlayer(Konane, Player):
    """
    Always chooses the first move from the set of possible moves.
    """
    def initialize(self, side):
        self.side = side
        self.name = "Simple"
    def getMove(self, board):
        moves = self.generateMoves(board, self.side)
        n = len(moves)
        if n == 0:
            return []
        else:
            return moves[0]

class RandomPlayer(Konane, Player):
    """
    Chooses a random move from the set of possible moves.
    """
    def initialize(self, side):
        self.side = side
        self.name = "Random"
    def getMove(self, board):
        moves = self.generateMoves(board, self.side)
        n = len(moves)
        if n == 0:
            return []
        else:
            return moves[random.randrange(0, n)]

class HumanPlayer(Player):
    """
    Prompts a human player for a move.
    """
    def initialize(self, side):
        self.side = side
        self.name = "Human"
    def getMove(self, board):
        r1, c1, r2, c2 = input("Enter r1, c1, r2, c2 (or -1's to concede): ")
        if r1 == -1:
            return []
        return [r1, c1, r2, c2]

class MinimaxPlayer(Konane, Player):
    def __init__(self, size, depthLimit):
        Konane.__init__(self, size)
        self.limit = depthLimit
        
    def initialize(self, side):
        self.side = side
        self.name = "WeightedAvailable"
        self.h = self.weightedAvailable
    
    def getMove(self, board):
        moves = self.generateMoves(board, self.side)
        n = len(moves)
        if n == 0:
            return []
        elif n == 1:
            return moves[0]
        else:
            return self.miniMaxDecision(board, moves)
    
    def miniMaxDecision(self, board, moves):
        alpha = -float("inf")
        best = moves[0]
        for m in moves:
            v = self.getMinValue(self.nextBoard(board, self.side, m), self.limit - 1, alpha, float("inf"))
            if v > alpha:
                best = m
                alpha = v
        return best
    
    def getMinValue(self, board, depth, alpha, beta):
        piece = self.opponent(self.side)
        if depth <= 0:
            return self.h(board)
        else:
            moves = self.generateMoves(board, piece)
            v = float("inf")
            for m in moves:
                v = min(v, self.getMaxValue(self.nextBoard(board, piece, m), depth -1, alpha, beta))
                if v <= alpha: return v
                beta = min(beta, v)
            return v
            
    def getMaxValue(self, board, depth, alpha, beta):
        piece = self.side
        if depth <= 0:
            return self.h(board)
        else:
            moves = self.generateMoves(board, piece)
            v = -float("inf")
            for m in moves:
                v = max(v, self.getMinValue(self.nextBoard(board, piece, m), depth - 1, alpha, beta))
                if v >= beta: return v
                alpha = max(alpha, v)
            return v
    
    def numPieces(self, side, board):
        return sum(map(lambda xs: xs.count(side), board))
    
    def movesAvailable(self, board):
        return len(self.generateMoves(board, self.side))
    
    def leastOpponentMoves(self, board):
        moves = self.generateMoves(board, self.opponent(self.side))
        if len(moves) == 0:
            return float("inf")
        else:
            return 1/len(moves)
    
    def combinedMovesMaxMin(self, board):
        if self.movesAvailable(board) == 0:
            return -float("inf")
        else:
            return self.leastOpponentMoves(board)
    
    def movesDifference(self, board):
        ourMoves = len(self.generateMoves(board, self.side))
        theirMoves = len(self.generateMoves(board, self.opponent(self.side)))
        if ourMoves == 0: return -float("inf")
        elif theirMoves == 0: return float("inf")
        else: return ourMoves - theirMoves
        
    def movesRatio(self, board):
        ourMoves = len(self.generateMoves(board, self.side))
        theirMoves = len(self.generateMoves(board, self.opponent(self.side)))
        if ourMoves == 0: return -float("inf")
        elif theirMoves == 0: return float("inf")
        else: return ourMoves / theirMoves
    
    def weightedAvailable(self, board):
        ourMoves = len(self.generateMoves(board, self.side))
        theirMoves = len(self.generateMoves(board, self.opponent(self.side)))
        ourPieces = self.numPieces(self.side, board)
        theirPieces = self.numPieces(self.opponent(self.side), board)
        if ourMoves == 0: return -float("inf")
        elif theirMoves == 0: return float("inf")
        else:
            return ourMoves*ourMoves + ourPieces - theirMoves*theirMoves - theirPieces
    
    
class RatioPlayer(Konane, Player):
    def __init__(self, size, depthLimit):
        Konane.__init__(self, size)
        self.limit = depthLimit
        
    def initialize(self, side):
        self.side = side
        self.name = "Ultimate Warrior"
        self.h = self.ultimateCombined
    
    def getMove(self, board):
        moves = self.generateMoves(board, self.side)
        n = len(moves)
        if n == 0:
            return []
        elif n == 1:
            return moves[0]
        else:
            return self.miniMaxDecision(board, moves)
    
    def miniMaxDecision(self, board, moves):
        alpha = -float("inf")
        best = moves[0]
        for m in moves:
            v = self.getMinValue(self.nextBoard(board, self.side, m), self.limit - 1, alpha, float("inf"))
            if v > alpha:
                best = m
                alpha = v
        return best
    
    def getMinValue(self, board, depth, alpha, beta):
        piece = self.opponent(self.side)
        if depth <= 0:
            return self.h(board)
        else:
            moves = self.generateMoves(board, piece)
            v = float("inf")
            for m in moves:
                v = min(v, self.getMaxValue(self.nextBoard(board, piece, m), depth -1, alpha, beta))
                if v <= alpha: return v
                beta = min(beta, v)
            return v
            
    def getMaxValue(self, board, depth, alpha, beta):
        piece = self.side
        if depth <= 0:
            return self.h(board)
        else:
            moves = self.generateMoves(board, piece)
            v = -float("inf")
            for m in moves:
                v = max(v, self.getMinValue(self.nextBoard(board, piece, m), depth - 1, alpha, beta))
                if v >= beta: return v
                alpha = max(alpha, v)
            return v
    
    def numPieces(self, side, board):
        return sum(map(lambda xs: xs.count(side), board))
        
    def numMoveable(self, side, board):
        return len(set(map(lambda mv: (mv[0],mv[1]), self.generateMoves(board, side))))
    
    def movesAvailable(self, board):
        return len(self.generateMoves(board, self.side))
    
    def leastOpponentMoves(self, board):
        moves = self.generateMoves(board, self.opponent(self.side))
        if len(moves) == 0:
            return float("inf")
        else:
            return 1/len(moves)
    
    def combinedMovesMaxMin(self, board):
        if self.movesAvailable(board) == 0:
            return -float("inf")
        else:
            return self.leastOpponentMoves(board)
    
    def movesDifference(self, board):
        ourMoves = len(self.generateMoves(board, self.side))
        theirMoves = len(self.generateMoves(board, self.opponent(self.side)))
        if ourMoves == 0: return -float("inf")
        elif theirMoves == 0: return float("inf")
        else: return ourMoves - theirMoves
        
    def movesRatio(self, board):
        ourMoves = len(self.generateMoves(board, self.side))
        theirMoves = len(self.generateMoves(board, self.opponent(self.side)))
        if ourMoves == 0: return -float("inf")
        elif theirMoves == 0: return float("inf")
        else: return ourMoves / theirMoves
        
    def weightedAvailable(self, board):
        ourMoves = len(self.generateMoves(board, self.side))
        theirMoves = len(self.generateMoves(board, self.opponent(self.side)))
        ourPieces = self.numPieces(self.side, board)
        theirPieces = self.numPieces(self.opponent(self.side), board)
        if ourMoves == 0: return -float("inf")
        elif theirMoves == 0: return float("inf")
        else:
            return ourMoves*20 + ourPieces - theirMoves*20 - theirPieces
    
    def ratioCombined(self, board):
        ourMoves = len(self.generateMoves(board, self.side))
        theirMoves = len(self.generateMoves(board, self.opponent(self.side)))
        ourPieces = self.numPieces(self.side, board)
        theirPieces = self.numPieces(self.opponent(self.side), board)
        if ourMoves == 0: return -float("inf")
        elif theirMoves == 0: return float("inf")
        else:
            return ourMoves*ourPieces/theirMoves/theirPieces
    
    def ourMoveable(self, board):
        ourNumMoveable = self.numMoveable(self.side, board)
        theirNumMoveable = self.numMoveable(self.opponent(self.side), board)
        
        if ourNumMoveable == 0: return -float("inf")
        elif theirNumMoveable == 0: return float("inf")
        else:
            return ourNumMoveable
    
    def moveableRatio(self, board):
        ourNumMoveable = self.numMoveable(self.side, board)
        theirNumMoveable = self.numMoveable(self.opponent(self.side), board)
        
        if ourNumMoveable == 0: return -float("inf")
        elif theirNumMoveable == 0: return float("inf")
        else:
            return ourNumMoveable/theirNumMoveable
    
    def ultimateCombined(self, board):
        ourMoves = len(self.generateMoves(board, self.side))
        theirMoves = len(self.generateMoves(board, self.opponent(self.side)))
        ourMoveable = self.numMoveable(self.side, board)
        theirMoveable = self.numMoveable(self.opponent(self.side), board)
        ourPieces = self.numPieces(self.side, board)
        theirPieces = self.numPieces(self.opponent(self.side), board)
        if theirMoves == 0: return float("inf")
        elif ourMoves == 0: return -float("inf")
        else:
            return ourMoves*ourMoves/2 + ourMoveable*2 + ourPieces - theirMoves*theirMoves/2 - theirPieces - theirMoveable*2
    

class MongoosePlayer(Konane, Player):
    def __init__(self, size, depthLimit):
        Konane.__init__(self, size)
        self.limit = depthLimit
        
    def initialize(self, side):
        self.side = side
        self.name = "Mongoose"
        self.h = self.movesWeightedParabolic
    
    def getMove(self, board):
        moves = self.generateMoves(board, self.side)
        n = len(moves)
        if n == 0:
            return []
        elif n == 1:
            return moves[0]
        else:
            return self.miniMaxDecision(board, moves)
    
    def miniMaxDecision(self, board, moves):
        alpha = -float("inf")
        best = moves[0]
        for m in moves:
            v = self.getMinValue(self.nextBoard(board, self.side, m), self.limit - 1, alpha, float("inf"))
            if v > alpha:
                best = m
                alpha = v
        return best
    
    def getMinValue(self, board, depth, alpha, beta):
        piece = self.opponent(self.side)
        if depth <= 0:
            return self.h(board)
        else:
            moves = self.generateMoves(board, piece)
            v = float("inf")
            for m in moves:
                v = min(v, self.getMaxValue(self.nextBoard(board, piece, m), depth -1, alpha, beta))
                if v <= alpha: return v
                beta = min(beta, v)
            return v
            
    def getMaxValue(self, board, depth, alpha, beta):
        piece = self.side
        if depth <= 0:
            return self.h(board)
        else:
            moves = self.generateMoves(board, piece)
            v = -float("inf")
            for m in moves:
                v = max(v, self.getMinValue(self.nextBoard(board, piece, m), depth - 1, alpha, beta))
                if v >= beta: return v
                alpha = max(alpha, v)
            return v
    
    def numPieces(self, side, board):
        return sum(map(lambda xs: xs.count(side), board))
    
    def movesAvailable(self, side, board):
        return len(self.generateMoves(board, side))
    
    def movesWeightedParabolic(self, board):
        ourMoves = self.movesAvailable(self.side, board)
        theirMoves = self.movesAvailable(self.opponent(self.side),board)
        ourPieces = self.numPieces(self.side, board)
        theirPieces = self.numPieces(self.opponent(self.side), board)
        if ourMoves == 0: return -float("inf")
        elif theirMoves == 0: return float("inf")
        else:
            return ourMoves*ourMoves + ourPieces - theirMoves*theirMoves - theirPieces
    
game = Konane(8)
game.playNGames(20, RandomPlayer(8), MongoosePlayer(8,4),0)

