#include <stdio.h>

/**esta funcção é a main
   @author miguel silva
   @version um

   esta funcção é a main
   é uma função muito importante
*/
int main()
{
   int first, second, add, subtract, multiply;
   float divide;
 
 /* Aqui pedimos ao utilizador que forneça
 dois números inteiros
  Printf imprime no ecra e scanf recolhe o input do utilizador
*/
   printf("Enter two integers\n");
   scanf("%d%d", &first, &second);
 
   add = first + second;
   subtract = first - second;
   multiply = first * second;
   divide = first / (float)second;   //typecasting
 
   printf("Sum = %d\n",add);
   printf("Difference = %d\n",subtract);
   printf("Multiplication = %d\n",multiply);
   printf("Division = %.2f\n",divide);
 
   return 0;
}