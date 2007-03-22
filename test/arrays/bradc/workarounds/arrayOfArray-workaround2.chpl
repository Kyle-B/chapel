config const n = 8;

const ProbDom = [1..n, 1..n],
      BigDom = [0..n+1, 0..n+1],
      StencDom = [-1..1, -1..1];

record arr33 {
  var data: [-1..1, -1..1] real;

  def this(x,y) var {
    return data(x,y);
  }

  def this(xy:2*int) var {
    return data(xy(1), xy(2));
  }
}

var A: [BigDom] real,
    B: [ProbDom] real,
    W: [ProbDom] arr33;

forall (i,j) in ProbDom {
  A(i,j) = (i-1)*n + j;
  forall (x,y) in StencDom {
    W(i,j)(x,y) = if (x == y) then 0.0 else 1.0;
  }
}

writeln("A is:\n", A, "\n");

forall ij in ProbDom do
  B(ij) = (+ reduce [off in StencDom] W(ij)(off)*A(ij+off)) / 6;

writeln("B is:\n", B, "\n");

// + on 2-tuples
def +(x:2*int, y:2*int) {
  return (x(1)+y(1), x(2)+y(2));
}

