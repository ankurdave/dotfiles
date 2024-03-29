int foo(int a,
        int b) {}
int foo(
    int a, int b) {}

// Make sure indentation works properly when starting a new line inside a parameter list. The
// indentation should be the same as if there were already another parameter.
int foo(int a,
        ) {}

int main(
    int a) {
  LOG(INFO) << "foo"
            << "bar";
  abc =
      b + c;
  abc = b + c
        + d + e;
  abcdefg + h + i
      + j + k;

  foo + (bar *
         baz);

  abc = foo ?
        bar : baz;

  DCHECK(foo ||
         bar)
      << "Failed";

  // Test empty block.
  if (abc) {
  }

  // Test statements with and without braces.
  if (abc)
    return foo;
  if (abc) {
    return foo;
  }

  for (int i = 0; i < 10; ++i)
    return foo;
  for (int i = 0; i < 10; ++i) {
    return foo;
  }

  while (abc)
    return foo;
  while (abc) {
    return foo;
  }

  do {
    foo();
  } while (abc);

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
