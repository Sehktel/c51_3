/* Реализация защитой от повторной инициализации */
/* Тест должен завершиться ошибкой*/
#include "test_25_singleton_guard.h"
#include "test_25_singleton_guard.h"

#include<reg2051.h>     
// Функция инициализации синглтона
void main(void) {
    char double_var;
    // Инициализация синглтона
    double_var = test_func();

    P1 = double_var;
    P3 = control_var;
}

// Функция проверки инициализации синглтона
char test_func(void) {
    return test_var;
} 