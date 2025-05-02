/* Заголовочный файл с защитой от повторного включения 
 * Реализация концепции "синглтона" на уровне препроцессора
 */
#ifndef TEST_25_SINGLETON_GUARD_H
#define TEST_25_SINGLETON_GUARD_H

// Глобальный флаг инициализации
#ifndef VAR_INITIALIZED
#define VAR_INITIALIZED

char test_var;
char test_func(void);
test_var = 10;

#else
    #error "Повторная попытка определения синглтона!"
    char control_var;
    control_var = 20;

#endif // CONTROL_VAR_INITIALIZED

#endif // TEST_25_SINGLETON_GUARD_H 