/* Работа с массивами */
void main(void) {
    char arr[5];
    char i;
    
    for(i = 0; i < 5; i = i + 1) {
        arr[i] = i;
    }
    
    arr[2] = arr[1] + arr[0];
} 