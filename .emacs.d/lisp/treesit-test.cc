int foo(int a,
        int b) {}
int foo(
    int a, int b) {}

int main(
    int a) {
  LOG(INFO) << "foo"
            << "bar";
  abc = b + c
        + d + e;
  abcdefg + h + i
      + j + k;

  foo + (bar *
         baz);

  DCHECK(foo ||
         bar)
      << "Failed";

  // Test empty block.
  if (abc) {
  }

  switch (a) {
    case 0:
      123;
    case 1: {
      456;
    }
    default:
  }
}

class A {
 private:
  A()
      : bar(0) {}
  A(int bar, int baz) :
      bar(bar),
      baz(baz) {}
  int foo();
};
