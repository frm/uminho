#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

int main(){
  char buf[100];
  int cpid;
  int* status;
  
  while(1){
    scanf("%[^\n]s\n", buf);
    while (getchar() != '\n');

    if( (cpid = fork()) == 0){
      printf("FILHO: %d\n", getpid() );
      printf("%s\n", buf);
      


    }
    else{
      waitpid(cpid, status, 0);
      printf("FILHO %d MORREU OH NOSE\n", cpid);
    }

  }
}
