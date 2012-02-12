void printInt(int a) {
	printf("%d\n", a);
}

void test(int*a, int pos) {
	printInt(a[pos]);
}

void main() {
	int a[100];
	a[10]=123;
	test(a,10);
}

