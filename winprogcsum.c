#include "winprogcsum.h"

#include <windows.h>
#include <stdio.h>

// TODO: test
unsigned long long programBinaryChecksum()
{
    TCHAR fname [1000];
    DWORD length = GetModuleFileName(NULL, fname, 1000);
    if(length == 1000)
    {
        // Fail
        return 0;
    }
    
    FILE * binary = fopen(fname, "r");
    if(!binary)
    {
        return 0;
    }
    
    unsigned long long checksum = 0;
    while(!feof(binary))
    {
        char buffer [256];
        size_t n = fread(buffer, 1, 256, binary);
        char * p;
        for(p = buffer; p < buffer + n; ++p)
        {
            checksum += *p;
        }
        if(ferror(binary))
        {
            return 0;
        }
    }
    fclose(binary);
    
    return checksum;
}
