#include <stdio.h>
#include <limits.h>

int main()
{
  int i = 0, *ip = &i;
  unsigned u = 0, *up = &u;
  unsigned int ui = 0, *uip = &ui;
  short s = 0, *sp = &s;
  unsigned short us = 0, *usp = &us;
  long l = 0, *lp = &l;
  unsigned long ul = 0, *ulp = &ul;
  long long ll = 0, *llp = &ll;
  unsigned long long ull = 0, *ullp = &ull;
  char c = 0, *cp = &c;
  unsigned char uc = 0, *ucp = &uc;
  float f = 0.0, *fp = &f;
  double d = 0.0, *dp = &d;

  printf("bits per char: %d\n", CHAR_BIT);
  printf("limits of          char:      [%d, %d]\n", CHAR_MIN, CHAR_MAX);
  printf("limits of   signed char:      [%d, %d]\n", SCHAR_MIN, SCHAR_MAX);
  printf("limits of unsigned char:      [0, %d]\n", UCHAR_MAX);
  printf("limits of          short:     [%d, %d]\n", SHRT_MIN, SHRT_MAX);
  printf("limits of unsigned short:     [0, %d]\n", USHRT_MAX);
  printf("limits of          int:       [%d, %d]\n", INT_MIN, INT_MAX);
  printf("limits of unsigned int:       [0, %u]\n", UINT_MAX);
  printf("limits of          long:      [%ld, %ld]\n", LONG_MIN, LONG_MAX);
  printf("limits of unsigned long:      [0, %lu]\n", ULONG_MAX);
  printf("limits of          long long: [%lld, %lld]\n", LLONG_MIN, LLONG_MAX);
  printf("limits of unsigned long long: [0, %llu]\n", ULLONG_MAX);
  printf("\n");
  printf("         char        c @ %p (%zu bytes): %d\n", cp, sizeof(c), c);
  printf("unsigned char       uc @ %p (%zu bytes): %d\n", ucp, sizeof(uc), uc);
  printf("         short       s @ %p (%zu bytes): %d\n", sp, sizeof(s), s);
  printf("unsigned short      us @ %p (%zu bytes): %d\n", usp, sizeof(us), us);
  printf("         int         i @ %p (%zu bytes): %d\n", ip, sizeof(i), i);
  printf("unsigned             u @ %p (%zu bytes): %d\n", up, sizeof(u), u);
  printf("unsigned int        ui @ %p (%zu bytes): %d\n", uip, sizeof(ui), ui);
  printf("         long        l @ %p (%zu bytes): %ld\n", lp, sizeof(l), l);
  printf("unsigned long       ul @ %p (%zu bytes): %lu\n", ulp, sizeof(ul), ul);
  printf("         long long  ll @ %p (%zu bytes): %lld\n", llp, sizeof(ll), ll);
  printf("unsigned long long ull @ %p (%zu bytes): %llu\n", ullp, sizeof(ull), ull);
  printf("         float       f @ %p (%zu bytes): %f\n", fp, sizeof(f), f);
  printf("         double      d @ %p (%zu bytes): %f\n", dp, sizeof(d), d);
}
