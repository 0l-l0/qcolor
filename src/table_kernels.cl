__kernel void fill_table(
		__global const uint *matrix,
		__global uint *work,
		__global const uint *nodes,
		__global const uint *color,
		__global const uint *range,
		__global const uint *slice)
{
	uint	offset = (*range) * get_global_id(0),
		first_m = (*slice) * offset,
		first_w = (*color) * offset,
		i=0, j, k, n, assist,
		count_w=0, count_c,
		nodes_in_partitions=(uint)ceil((float)(*nodes)/(float)(*color));
	const uint mask=2147483648;//pow(2,31);

	for(;i<(*range) && (assist=offset+i)<(*nodes);++i) {
		n=0;
		count_c=0;
		for(j=0;j<(*slice);++j) {
			for(k=0;k<32;++k) {
				if(nodes_in_partitions==count_c++ && j*32+k<(*nodes)) {
					if(floor(assist/nodes_in_partitions)==n) count_w|=mask;
					work[first_w+i*(*color)+n++]=count_w;
					count_w=0;
					count_c=1;
				}
				else if(j*32+k==(*nodes)) {
					if(floor(assist/nodes_in_partitions)==n) count_w|=mask;
					work[first_w+i*(*color)+n++]=count_w;
					k=32;
				}
				if(matrix[first_m+i*(*slice)+j] & (mask>>k)) ++count_w;
			}
		}
	}
}

__kernel void max_search(
		__global uint *work,
		__global uint *marray,
		__global const uint *nodes,
		__global const uint *color,
		__global const uint *range)
{
	uint	gid = get_global_id(0),
		offset = (*range) * gid,
		first_w = (*color) * offset,
		i=0, j, mini, flaged,
		assist1, assist2, assist3, assist4, assist5,
		max_diff[]={0,0,0,0};
	const uint mask=2147483648;//pow(2,31);

	for(;i<(*range) && (assist3=offset+i)<(*nodes);++i) {
		mini=4294967295; // full with ones
		for(j=0;j<(*color);++j) {
			assist1=work[first_w+i*(*color)+j];
			if(assist1 & mask) {
				flaged=assist1^mask;
				assist4=j;
			}
			else if(assist1<mini) {
				mini=assist1;
				assist5=j;
			}
		}
		if(flaged>mini && (assist2=flaged-mini)>max_diff[1]) {
			max_diff[0]=assist3;
			max_diff[1]=assist2;
			max_diff[2]=assist4;
			max_diff[3]=assist5;
		}
	}
	marray[gid*4]=max_diff[0];
	marray[gid*4+1]=max_diff[1];
	marray[gid*4+2]=max_diff[2];
	marray[gid*4+3]=max_diff[3];
}

__kernel void alter(
		__global uint *work,
		__global const uint *pivot,
		__global const uint *nodes,
		__global const uint *color,
		__global const uint *range)
{
	uint	offset = (*range) * get_global_id(0),
		first_w = (*color) * offset,
		i=0, assist1, assist2, temp_mask;
	const uint mask=2147483648;//pow(2,31);

	for(;i<(*range) && (assist1=offset+i)<(*nodes);++i) {
		if(assist1==pivot[0]) {
			work[first_w+i*(*color)+pivot[1]]^=mask;
			work[first_w+i*(*color)+pivot[2]]|=mask;
		}
		else {
			assist2=floor(assist1/32);
			temp_mask=pow(2,31-assist1+assist2*32);
			if(pivot[3+assist2] & temp_mask) {
				--work[first_w+i*(*color)+pivot[1]];
				++work[first_w+i*(*color)+pivot[2]];
			}
		}
	}
}

__kernel void check(
		__global uint *work,
		__global uint *marray,
		__global const uint *nodes,
		__global const uint *color,
		__global const uint *range)
{
	uint	gid = get_global_id(0),
		offset = (*range) * gid,
		first_w = (*color) * offset,
		i=0, j, assist;
	const uint mask=2147483648;//pow(2,31);

	marray[gid]=0;
	for(;i<(*range) && offset+i<(*nodes);++i) {
		for(j=0;j<(*color);++j) {
			assist=work[first_w+i*(*color)+j];
			if(assist & mask) {
				if(assist ^ mask) {
					marray[gid]=4294967295; // not null value
					i=(*range);
				}
				j=(*color);
			}
		}
	}
}
