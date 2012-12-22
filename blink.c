#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

char *on = "0 on";
char *off = "0 off";

int main () {
  int fd, i;
  setuid(0);
  setgid(0);
  if ((fd = open("/proc/acpi/ibm/led", O_WRONLY)) == -1) _exit(0);
  for (i = 0; i < 10; i++) {
    write(fd, off, strlen(off));
    usleep(200000);
    write(fd, on, strlen(on));
    usleep(200000);
  }
  close(fd);
}
