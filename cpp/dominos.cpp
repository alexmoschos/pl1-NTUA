#include <iostream>
#include <stdio.h>
#include <stdlib.h>
using namespace std;

int result = 0;
int a[7][8];
bool domino[7][7] = {0};
bool used[7][8] = {0};

void fix(int &a, int &b) {
    if (b > a) {
        int temp = a;
        a = b;
        b = temp;
    }
}

int exists(int i, int j) { return ((i <= 8) && (j <= 7) && !used[i][j]); }
void count(int i, int j) {
    int first, second;
    if (j == 8) {
        j = 0;
        ++i;
    }
    if (i == 7) {
        // printf("yes");
        ++result;
        return;
    }

    if (used[i][j])
        count(i, j + 1);
    else {
        if ((j + 1 < 8) && !used[i][j + 1] && !domino[a[i][j]][a[i][j + 1]]) {
            first = a[i][j];
            second = a[i][j + 1];
            domino[first][second] = 1;
            domino[second][first] = 1;
            used[i][j + 1] = used[i][j] = 1;
            count(i, j + 2);
            used[i][j + 1] = used[i][j] = 0;
            domino[first][second] = 0;
            domino[second][first] = 0;
        }
        if ((i + 1 < 7) && !used[i + 1][j] && !domino[a[i][j]][a[i + 1][j]]) {
            first = a[i][j];
            second = a[i + 1][j];
            domino[first][second] = 1;
            domino[second][first] = 1;
            used[i + 1][j] = used[i][j] = 1;
            count(i, j + 1);
            used[i + 1][j] = used[i][j] = 0;
            domino[first][second] = 0;
            domino[second][first] = 0;
        }
    }
}
int main(int argc, char const *argv[]) {

    freopen(argv[1], "r", stdin); // redirects standard input

    for (int i = 0; i < 7; ++i) {
        for (int j = 0; j < 8; ++j) {
            cin >> a[i][j];
        }
    }

    count(0, 0);
    cout << result << endl;
    return 0;
}
