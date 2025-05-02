/* Указатели на функции */
char add(char x) {
    return x + 1;
}

char sub(char x) {
    return x - 1;
}

void main(void) {
    char a;
    char (*fptr)(char);  // Указатель на функцию
    
    fptr = add;
    a = fptr(5);     // Вызов через указатель
    
    fptr = sub;
    a = fptr(5);     // Вызов через указатель
} 
/* Да, это тоже компилируется О.О */