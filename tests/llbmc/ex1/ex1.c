int a[3];

void foo(int m)
{
  int i;

  for (i = 0; i < m; i++)
    a[i] = i;  // memory access violation, if m > 3
}
