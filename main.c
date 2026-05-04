#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <string.h>
#include <time.h>

#define NUM_CLIENTES 100
#define NUM_VEICULOS 30

typedef struct{
    int id;
    float x, y;
    float demanda;
    int visitado;
} Cliente;

typedef struct {
    int    sequencia[NUM_CLIENTES + 2]; 
    int    tamanho;
    double carga_atual;
    double consumo_total;   
    double distancia_total;
} Rota;

double distancia (Cliente clie1, Cliente clie2){
    double dist_x = clie1.x - clie2.x;
    double dist_y = clie1.y - clie2.y;
    return sqrt(dist_x * dist_x + dist_y * dist_y);
}

// a - consumo do veículo vazio
// b - impacto da carga
 // Bektas & Laporte, 2011
double consumo_aresta(double dist, double carga) {
    double a = 0.3;
    double b = 0.001;

    return dist * (a + b * carga);
}

void ler_instancia(const char *arquivo, Cliente clientes[], int *n, double *capacidade){
    FILE *f = fopen(arquivo, "r");
    if (!f){
        printf("Erro ao abrir arquivo\n");
        //return 0;
    }

    char linha[256];
    int lendo_coord = 0, lendo_demanda = 0;

    while (fgets(linha, sizeof(linha), f)){
        if (strstr(linha, "DIMENSION")){
            sscanf(linha, "DIMENSION : %d", n);
        }
        else if (strstr(linha, "CAPACITY")){
            sscanf(linha, "CAPACITY : %lf", capacidade);
        }
        else if (strstr(linha, "NODE_COORD_SECTION")){
            lendo_coord = 1;
            continue;
        }
        else if (strstr(linha, "DEMAND_SECTION")){
            lendo_coord = 0;
            lendo_demanda = 1;
            continue;
        }
        else if (strstr(linha, "DEPOT_SECTION")){
            break;
        }

        if (lendo_coord){
            int id;
            double x,y;
            sscanf(linha, "%d %lf %lf", &id, &x, &y);
            clientes[id-1].id = id-1;
            clientes[id-1].x = x;
            clientes[id-1].y = y;
            clientes[id-1].visitado = (id == 1);
        }

        if (lendo_demanda){
            int id;
            double d;
            sscanf(linha, "%d %lf", &id, &d);
            clientes[id-1].demanda = d;
        }
    }

    fclose(f);
}

int guloso_gvrp(Cliente clientes[],int num_clientes, double capacidade, Rota rotas[], int *num_rotas, int modo_gvrp){
    int visitados = 0;
    *num_rotas = 0;

    while(visitados < num_clientes - 1){
        Rota *rota_atual = &rotas[*num_rotas];
        rota_atual->tamanho = 0;
        rota_atual->carga_atual = 0.0;
        rota_atual->consumo_total = 0.0;
        rota_atual->distancia_total = 0.0;

        rota_atual->sequencia[rota_atual->tamanho++] = 0;
        int pos_atual = 0;
        int inseriu = 0;
        do{
            inseriu = 0;
            int melhor_idx = -1;
            double melhor_custo = INFINITY;
            double melhor_dist = 0.0;

            for(int i = 1; i < num_clientes; i++){
                if (clientes[i].visitado) continue;
                if (clientes[i].demanda + rota_atual->carga_atual > capacidade) continue;

                double dist = distancia(clientes[pos_atual], clientes[i]);
                double carga_veiculo = capacidade - rota_atual->carga_atual;
                double custo = 0.0;

                if (modo_gvrp == 1) {
                    custo = consumo_aresta(dist, carga_veiculo);
                }else {
                    custo = dist; 
                }

                if (custo < melhor_custo){
                    melhor_custo = custo;
                    melhor_dist = dist;
                    melhor_idx = i;
                }
            }

            if (melhor_idx != -1) {
                clientes[melhor_idx].visitado = 1;
                rota_atual->carga_atual += clientes[melhor_idx].demanda;

                rota_atual->consumo_total += melhor_custo;
                rota_atual->distancia_total += melhor_dist;
                rota_atual->sequencia[rota_atual->tamanho++] = melhor_idx;

                pos_atual = melhor_idx;
                visitados++;
                inseriu = 1;
            }
        } while(inseriu);

        double dist_retorno = distancia(clientes[pos_atual], clientes[0]);
        rota_atual->distancia_total += dist_retorno;
        rota_atual->consumo_total += consumo_aresta(dist_retorno, rota_atual->carga_atual);
        rota_atual->sequencia[rota_atual->tamanho++] = 0;

        (*num_rotas)++;
    }
    return 0;
}

void reset_visitados(Cliente clientes[], int n){
    for (int i=1;i<n;i++){
        clientes[i].visitado = 0;
    } 
}

int main(){
    Cliente clientes[NUM_CLIENTES];
    Rota rotas_gvrp[NUM_VEICULOS], rotas_vrp[NUM_VEICULOS];

    int n;
    double capacidade;

    ler_instancia("tai75b.vrp", clientes, &n, &capacidade);

    int n_gvrp, n_vrp;

    guloso_gvrp(clientes, n, capacidade, rotas_gvrp, &n_gvrp, 1);


    double consumo_gvrp = 0;
    for (int i=0; i < n_gvrp; i++){
        consumo_gvrp += rotas_gvrp[i].consumo_total;
    } 

    reset_visitados(clientes, n);

    guloso_gvrp(clientes, n, capacidade, rotas_vrp, &n_vrp, 0);

    int nao_visitados = 0;

    for (int i = 1; i < n; i++) {
        if (!clientes[i].visitado) {
            nao_visitados++;
        }
    }

    printf("\nClientes nao visitados: %d\n", nao_visitados);

    double consumo_vrp = 0;
    for (int i=0;i<n_vrp;i++){
        consumo_vrp += rotas_vrp[i].consumo_total;
    } 

    double economia = ((consumo_vrp - consumo_gvrp)/consumo_vrp) * 100.0;

    printf("=== RESULTADOS ===\n\n");
    printf("GVRP -> Combustivel: %.2f L | Veiculos: %d\n", consumo_gvrp, n_gvrp);
    printf("VRP  -> Combustivel: %.2f L | Veiculos: %d\n", consumo_vrp, n_vrp);
    printf("Economia: %.2f%%\n", economia);

    return 0;
}
