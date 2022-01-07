#include <stdio.h>

#include "ldb.h"
#include "os.h"

void os_zero(addr, length)
os_vm_address_t addr;
os_vm_size_t length;
{
    os_vm_address_t block_start;
    os_vm_size_t block_size;

#ifdef DEBUG
    fprintf(stderr,";;; os_zero: addr: 0x%08x, len: 0x%08x\n",addr,length);
#endif

    block_start=os_round_up_to_page(addr);

    length-=block_start-addr;
    block_size=os_trunc_size_to_page(length);

    if(block_start>addr)
	bzero((char *)addr,block_start-addr);
    if(block_size<length)
	bzero((char *)block_start+block_size,length-block_size);
    
    if (block_size != 0) {
	/* Now deallocate and allocate the block so that it */
	/* faults in  zero-filled. */

	os_invalidate(block_start,block_size);
	addr=os_validate(block_start,block_size);

	if(addr==NULL || addr!=block_start)
	    fprintf(stderr,"os_zero: block moved, 0x%08x ==> 0x%08x!\n",block_start,addr);
    }
}

os_vm_address_t os_allocate(len)
os_vm_size_t len;
{
    return os_validate((os_vm_address_t)NULL,len);
}

os_vm_address_t os_allocate_at(addr,len)
os_vm_address_t addr;
os_vm_size_t len;
{
    return os_validate(addr, len);
}

void os_deallocate(addr,len)
os_vm_address_t addr;
os_vm_size_t len;
{
    os_invalidate(addr,len);
}

os_vm_address_t os_reallocate(addr,old_len,len)
os_vm_address_t addr;
os_vm_size_t old_len, len;
{
    addr=os_trunc_to_page(addr);
    len=os_round_up_size_to_page(len);
    old_len=os_round_up_size_to_page(old_len);

    if(addr==NULL)
	return os_allocate(len);
    else{
	long len_diff=len-old_len;

	if(len_diff<0)
	    os_invalidate(addr+len,-len_diff);
	else if(len_diff!=0){
	    os_vm_address_t new=os_validate(addr+old_len,len_diff);

	    if(new==NULL || new!=addr+old_len){
		if(new!=NULL)
		    /* allocated alright, but in the wrong place */
		    os_invalidate(new,len_diff);

		new=os_allocate(len);
		
		if(new!=NULL){
		    bcopy(addr,new,old_len);
		    os_invalidate(addr,old_len);
		}
		
		addr=new;
	    }
	}
	
	return addr;
    }
}
