#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

int min(int a, int b) { return (a < b) ? a : b; }
int abs(int a) { return (a > 0) ? a : 0; }

void construct(int i, int j, int iasks, int jasks, int igives, int jgives,
               int oldigives, int oldjasks, int *M, int *N, int *flag1) {
    int k;
    while (((i - j) > 1) && !(*flag1)) {

        iasks = oldigives;
        jgives = oldjasks;
        if (abs(M[i] - M[j]) <= 1) {
            igives = jgives;
            k = min(M[i], M[j]);
            if (M[i] == M[j]) {
                jasks = iasks;
            } else {
                jasks = 1 - iasks;
                if (((M[i] > M[j]) && (jasks)) || ((M[i] < M[j]) && (iasks)))
                    *flag1 = 1;
            }
            if (igives) {
                if (iasks && jasks) {
                    N[i] = 9;
                    N[j] = k;
                } else {
                    if (M[i] == 9 && M[j] == 9 && !iasks && !jasks)
                        *flag1 = 1;
                    N[i] = 9;
                    N[j] = k + 1;
                }
            } else {
                if (iasks && jasks) {
                    if (M[i] == 0 && M[j] == 0)
                        *flag1 = 1;
                    N[i] = k - 1;
                    N[j] = 0;
                } else {
                    N[i] = k;
                    N[j] = 0;
                }
            }
            oldjasks = jasks;
            oldigives = igives;
            // printf("%d%d ",N[j],N[i]);
        } else if (M[i] == 0 && M[j] == 9 && iasks && (!jgives)) {
            igives = 1;
            jasks = 0;
            N[i] = 9;
            N[j] = 0;
            oldjasks = jasks;
            oldigives = igives;
            // printf("%d%d ",N[j],N[i]);
        } else if (M[i] == 9 && M[j] == 0 && !iasks && jgives) {
            jasks = 1;
            igives = 0;
            N[i] = 9;
            N[j] = 0;
            oldjasks = jasks;
            oldigives = igives;
            // printf("%d%d ",N[j],N[i]);
        } else
            *flag1 = 1;
        i--;
        j++;
    }
    if (!(*flag1)) {
        if ((i - j) == 1) {
            iasks = oldigives;
            jgives = oldjasks;
            if (abs(M[i] - M[j]) <= 1) {
                igives = jgives;
                if (M[i] == M[j])
                    jasks = iasks;
                else {
                    jasks = 1 - iasks;
                    if (((M[i] > M[j]) && (jasks)) ||
                        ((M[i] < M[j]) && (iasks)))
                        *flag1 = 1;
                }
                k = min(M[i], M[j]);
                if (jasks == igives) {
                    if (igives) {
                        if (iasks && jasks) {
                            N[i] = 9;
                            N[j] = k;
                        } else {
                            N[i] = 9;
                            N[j] = k + 1;
                        }
                    } else {
                        if (iasks && jasks) {
                            N[i] = k - 1;
                            N[j] = 0;
                        } else {
                            N[i] = k;
                            N[j] = 0;
                        }
                    }
                    // printf("%d%d ",N[j],N[i]);
                } else
                    *flag1 = 1;
            } else if (M[i] == 0 && M[j] == 9 && iasks && (!jgives)) {
                igives = 1;
                jasks = 0;
                if (jasks == igives) {
                    N[i] = 9;
                    N[j] = 0;
                    // printf("%d%d ",N[j],N[i]);
                } else
                    *flag1 = 1;
            } else if (M[i] == 9 && M[j] == 0 && !iasks && jgives) {
                jasks = 1;
                igives = 0;
                if (jasks == igives) {
                    N[i] = 9;
                    N[j] = 0;
                    // printf("%d%d ",N[j],N[i]);
                }
            }

        } else if (i == j) {
            iasks = oldigives;
            jgives = oldjasks;
            if ((iasks && (M[i] % 2 == 0)) || (!iasks && (M[i] % 2 != 0)))
                *flag1 = 1;
            N[i] = 5 * jgives + (M[i] - iasks) / 2;
            // printf("%d ",N[i]);
        }
    }
    return;
}

int main(int argc, char *argv[]) {
    clock_t start, end;
    double cpu_time_used;
    start = clock();
    unsigned long long sizem, sizen;
    FILE *fin = fopen(argv[1], "r"), *fout = fopen("result.txt", "w");
    int *N = (int *)malloc(1000000 * sizeof(int)),
        *M = (int *)malloc(1000000 * sizeof(int));
    int a, i = 0, j;
    int igives, iasks, jgives, jasks, k, flag1 = 0, oldigives, oldjasks;
    while ((a = fgetc(fin)) != EOF) {
        M[i] = (char)a - '0';
        i++;
    }
    sizem = i;
    // for(i=0;i<sizem;i++)printf("%d",M[i]);
    if (sizem == 1) {
        if (M[0] % 2 == 0)
            fprintf(fout, "%d", M[0] / 2);
        else
            fprintf(fout, "0");
    } else if (sizem == 2) {
        if (M[0] == 1) {
            if ((10 + M[1]) % 2 == 0)
                fprintf(fout, "%d", (10 + M[1]) / 2);
            else
                fprintf(fout, "0");
        } else {
            if (M[i] != M[j])
                fprintf(fout, "0");
            else
                fprintf(fout, "%d%d", (M[i] - 1), 1);
        }
    } else if (M[0] != 1) {
        i = sizem - 1;
        j = 0;
        iasks = 0;
        igives = 0;
        jgives = 0;
        if ((abs(M[i] - M[j]) > 1))
            flag1 = 1;
        if (!(M[i] == 9 && M[j] == 0) && (!flag1)) {
            if (M[i] > M[j])
                flag1 = 1;
            oldigives = igives;
            if (M[i] == M[j]) {
                jasks = 0;
            } else {
                jasks = 1;
            }
            N[j] = 1;
            N[i] = M[i] - 1;
            // printf("%d%d ",N[j],N[i]);
            oldjasks = jasks;
            i--;
            j++;
            construct(i, j, iasks, jasks, igives, jgives, oldigives, oldjasks,
                      M, N, &flag1);
        }
        // printf("Number ");
        if (flag1)
            fprintf(fout, "0");
        else
            for (i = 0; i < sizem; i++)
                fprintf(fout, "%d", N[i]);
    } else { // two possibilities when M[0]=1
        j = 1;
        i = sizem - 1;
        jgives = 1;
        iasks = 0;
        if (abs(M[i] - M[j]) <= 1) {
            // printf("yes");
            k = min(M[i], M[j]);
            igives = jgives;
            if (M[i] > M[j])
                flag1 = 1;
            if (M[i] == M[j])
                jasks = 0;
            else
                jasks = 1;
            N[j] = 9;     // 5+((M[i]>0)?1:0);
            N[i] = k + 1; // 5+((M[i]>0)?(M[i]-1):0);
            // printf("%d %d \n",N[j],N[i]);
            oldigives = igives;
            oldjasks = jasks;
        } else if (M[i] == 9 && M[j] == 0) {
            jasks = 1;
            igives = 0;
            oldigives = igives;
            oldjasks = jasks;
            N[j] = 9;
            N[i] = 0;
            // printf("%d%d ",N[j],N[i]);
        } else
            flag1 = 1;
        i--;
        j++;
        construct(i, j, iasks, jasks, igives, jgives, oldigives, oldjasks, M, N,
                  &flag1);
        if (flag1) { // second possibility sizen=sizem
            j = 0;
            i = sizem - 1;
            jgives = igives = 0;
            iasks = 0;
            flag1 = 0;
            if (abs(M[i] - M[j]) > 1)
                flag1 = 1;
            else {
                if (M[i] > M[j])
                    flag1 = 1;
                if (M[i] == M[j])
                    jasks = 0;
                else
                    jasks = 1;
                N[j] = (M[i] > 0) ? 1 : 0;
                N[i] = 0;
                // printf("%d%d ",N[j],N[i]);
                oldigives = igives;
                oldjasks = jasks;
            }
            i--;
            j++;
            construct(i, j, iasks, jasks, igives, jgives, oldigives, oldjasks,
                      M, N, &flag1);
            // printf("Number ");
            if (flag1)
                fprintf(fout, "0");
            else
                for (i = 0; i < sizem; i++)
                    fprintf(fout, "%d", N[i]);
        } else {
            // printf("Number ");
            // printf("yes");
            if (flag1)
                fprintf(fout, "0");
            else
                for (i = 1; i < sizem; i++)
                    fprintf(fout, "%d", N[i]);
        }
    }
    fclose(fin);
    fclose(fout);
    end = clock();
    cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
    // printf("time:%lf",cpu_time_used);
    return 0;
}