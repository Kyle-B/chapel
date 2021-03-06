use myrecord;

proc make(i:int) {
  var ret: R;
  ret.init(x = i);
  ret.verify();
  return ret;
}


iter myiter() {
  var tmp = make(1);
  yield tmp.x;
  var tmp2 = make(2);
  yield tmp2.x;

}

proc run() {
  var i = 1;
  for x in myiter() {
    assert(x == i);
    i += 1;
  }
}

run();
verify();
