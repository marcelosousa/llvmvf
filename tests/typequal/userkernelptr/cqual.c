unsigned long copy_from_user(void *, const void *, int);
unsigned long copy_to_user(const void *, void *, int);

int x;

void sys_setint(int *p) {
	copy_from_user(&x, p, sizeof(x));
}

void sys_getint(int *p) {
	copy_to_user(p, &x, sizeof(x));
}

