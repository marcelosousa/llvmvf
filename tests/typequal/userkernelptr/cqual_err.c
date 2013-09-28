void * _memcpy(void *, const void *, unsigned long);

int x;

/*
void sys_setint(int *p) {
	_memcpy(&x, p, sizeof(x)); // BAD! 
}
*/

void sys_getint(int *p) {
	_memcpy(p, &x, sizeof(x)); // BAD! 
}

