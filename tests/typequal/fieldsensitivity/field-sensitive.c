// Example

void *ioremap(unsigned long phys_addr, unsigned long size);
void *__kmalloc(unsigned long size, int flags);

typedef struct Device
{
	void * io_addr;
	void * mem_addr;
}Device;


Device *f(Device *x){
	x->io_addr = ioremap(1024,512);
	x->mem_addr = __kmalloc(512,0);
	return x;
}