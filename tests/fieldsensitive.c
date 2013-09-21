struct X{
  int x;
  int y;
};

int foo();
int bar();

struct X gogo(){
  int x = foo();
  int y = bar();
  struct X f = {x,y};
  return f;
}
