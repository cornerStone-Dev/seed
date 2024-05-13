
MEMORY
{
    ram : ORIGIN = 0x20000000, LENGTH = 0x10000
    /*ram : ORIGIN = 0x2000000, LENGTH = 0x10000*/
}

ENTRY(reset)

SECTIONS
{
    .text   : { *(.text*)   } > ram
    
    .rodata  : {
        . = ALIGN(4);
        __rodata_start__ = .;
        *(SORT_BY_ALIGNMENT(SORT_BY_NAME(.rodata*)))
        . = ALIGN(4);
        __rodata_end__ = .;
    } > ram
    
    .data  : {
        . = ALIGN(4);
        __data_start__ = .;
        *(SORT_BY_ALIGNMENT(SORT_BY_NAME(.data*)))
        . = ALIGN(4);
        __data_end__ = .;
    } > ram
    
    .bss  : {
        . = ALIGN(4);
        __bss_start__ = .;
        *(SORT_BY_ALIGNMENT(.bss*))
        *(COMMON)
        . = ALIGN(4);
        __bss_end__ = .;
    } > ram
}
