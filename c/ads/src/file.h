#ifndef _FILE_H_
#define _FILE_H_

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>

/*
 * Parameters:
 * - in file_name: a string containing the file name of the file to be opened.
 * - out fd: a pointer to an int which will hold the file descriptor.
 * - out file_size: a pointer to an int which will hold the file size.
 * - out data: a pointer to a pointer to a char, which will hold the starting
 *   address of the file's data.
 * Return value: en error code, 0 if successful, -1 otherwise.
 * Description: opens a file using mmap, setting a pointer parameter to the
 * address of the start of the file's memory-mapped contents.
 */
int open_file(const char *file_name, int *fd, int *file_size, char **data) {
  struct stat sb;

  int _fd = open(file_name, O_RDONLY);
  if (_fd == -1) {
    return -1;
  }

  int _status = fstat(_fd, &sb);
  if (_status == -1) {
    return -1;
  }

  int _file_size = sb.st_size;
  char *_data = mmap(0, _file_size, PROT_READ, MAP_SHARED, _fd, 0);
  if (_data == NULL) {
    return -1;
  }

  *fd = _fd;
  *file_size = _file_size;
  *data = _data;
  return 0;
}

#endif
