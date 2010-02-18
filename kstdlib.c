#include <stdio.h>

/* putchard - putchar that takes a double and returns 0. */
extern double print(double x) {
  printf("%0.3f\n", x);
  return 0.0;
}
