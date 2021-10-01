import sympy as sym
# from sympy.abc import a, b, c, d
from sympy import Matrix, symbols

a, b, c, d = symbols('a b c d')
C, I, G, Yt = symbols('C I G Yt')
Yt_1, Yt_2, Gt_1 = symbols('Yt_1 Yt_2 Gt_1')
e, u, v = symbols('e u v')


# A = Matrix([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [-1, -1, -1, 1]])

# B = Matrix([[-a, 0, 0, -b], [-c, c, 0, 0], [0, 0, -d, 0], [0, 0, 0, 0]])

# A_t = Transpose(A)
# Yt_1, Yt_2, Gt_1 = symbols('Yt_1 Yt_2 Gt_1')

A = Matrix([
    [1, 0, 0, 0],
    [0, 1, 0, 0],
    [0, 0, 1, 0],
    [-1, -1, -1, 1]
])

B = Matrix([
    [-b, -a, 0, 0],
    [0, -c, c, 0],
    [0, 0, 0, -d],
    [0, 0, 0, 0]
])

Y = Matrix([
    [C],
    [I],
    [G],
    [Yt]
])

X = Matrix([
    [1],
    [Yt_1],
    [Yt_2],
    [Gt_1]
])

U = Matrix([
    [e],
    [u],
    [v],
    [0]
])

# print(Y)

M = -A.inv() * B
print(M)

A_U = A.inv() * U

_Y = M * X + A_U
print(_Y)

#
# print(X)
#
# print(A.inv())
#
# print(U)
