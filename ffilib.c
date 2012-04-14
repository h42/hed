#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/ioctl.h>

int win_size (int *ans) {
    struct winsize win;
    int rc = ioctl (1, TIOCGWINSZ, (char *) &win);
    if (!rc) {
	ans[0]=win.ws_row;
	ans[1]=win.ws_col;
    }
    return rc;
}

int getfilemode(char *fn) {
    struct stat st;
    int rc = stat(fn, &st);
    int mode = (rc==0) ? st.st_mode & 07777 : 0;
    //printf("%d %o\n",mode,mode);
    return mode;
}

int setfilemode(char *fn, int mode) {
    return chmod(fn, mode);
}

int c_readlink(char *fn, char *fn2, int size) {
    int rc;
    struct stat st1;
    rc=lstat(fn,&st1);
    // return fn if stat fails(file does not exist???) or not symlink
    if (rc) return rc;
    if (!S_ISLNK(st1.st_mode)) return 0;
    rc=readlink(fn,fn2,size);
    if (rc>0 && rc<size) {
	fn2[rc]=0;
	return rc;
    }
    else return -1;
} 

