###   File: mongoose.py
### Author: Mervin Fansler
###   Date: 20 April 2014
### Classes defined: MongoosePlayer

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
