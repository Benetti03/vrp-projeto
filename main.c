#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>

#define NUM_CLIENTES 75
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

const double LIMIAR_MAX = 1e-9;

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

int guloso_gvrp(Cliente clientes[], int num_clientes, double capacidade, Rota rotas[], int *num_rotas, int modo_gvrp, int max_veiculos) {
    int visitados = 0;
    *num_rotas = 0;

    while (visitados < num_clientes - 1) {
        int ultimo_veiculo = (*num_rotas >= max_veiculos - 1);

        Rota *rota_atual = &rotas[*num_rotas];
        rota_atual->tamanho        = 0;
        rota_atual->carga_atual    = 0.0;
        rota_atual->consumo_total  = 0.0;
        rota_atual->distancia_total = 0.0;

        rota_atual->sequencia[rota_atual->tamanho++] = 0;
        int pos_atual = 0;
        int inseriu   = 0;

        do {
            inseriu = 0;
            int melhor_idx = -1;
            double melhor_custo = INFINITY;
            double melhor_dist = 0.0;

            for (int i = 1; i < num_clientes; i++) {
                if (clientes[i].visitado) continue;

                if (!ultimo_veiculo && (clientes[i].demanda + rota_atual->carga_atual > capacidade)) continue;

                double dist = distancia(clientes[pos_atual], clientes[i]);

                double carga_na_aresta = rota_atual->carga_atual + clientes[i].demanda;
                double custo = 0.0;
                custo = (modo_gvrp == 1) ? consumo_aresta(dist, carga_na_aresta) : dist;

                if (custo < melhor_custo) {
                    melhor_custo = custo;
                    melhor_dist  = dist;
                    melhor_idx   = i;
                }
            }

            if (melhor_idx != -1) {
                double carga_na_aresta = rota_atual->carga_atual + clientes[melhor_idx].demanda;

                clientes[melhor_idx].visitado = 1;
                rota_atual->carga_atual += clientes[melhor_idx].demanda;

                rota_atual->consumo_total += consumo_aresta(melhor_dist, carga_na_aresta);
                rota_atual->distancia_total += melhor_dist;
                rota_atual->sequencia[rota_atual->tamanho++] = melhor_idx;

                pos_atual = melhor_idx;
                visitados++;
                inseriu = 1;
            }
        } while (inseriu);

        double dist_retorno = distancia(clientes[pos_atual], clientes[0]);
        rota_atual->distancia_total += dist_retorno;
        rota_atual->consumo_total   += consumo_aresta(dist_retorno, rota_atual->carga_atual);
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

/* PARTE 2 DO TRABALHO */
void recalcula_rota(Rota *r, Cliente clientes[]) {
    r->consumo_total   = 0.0;
    r->distancia_total = 0.0;
    r->carga_atual     = 0.0;
 
    for (int i = 1; i < r->tamanho; i++) {
        int idx = r->sequencia[i];
        r->carga_atual += clientes[idx].demanda;
 
        double d = distancia(clientes[r->sequencia[i-1]], clientes[idx]);
        r->distancia_total += d;
        r->consumo_total   += consumo_aresta(d, r->carga_atual);
    }
}

/* Heurística de busca local 2-opt Referências pro artigo: Croes (1958), Lin (1965), Bektas & Laporte (2011) — modelo de consumo*/
void dois_opt(Rota *r, Cliente clientes[]) {
    int melhorou = 1;

    while (melhorou) {
        melhorou = 0;

        for (int i = 1; i < r->tamanho - 2; i++) {
            for (int j = i + 1; j < r->tamanho - 1; j++) {

                double carga_antes_i = 0.0;
                for (int k = 1; k < i; k++){
                    carga_antes_i += clientes[r->sequencia[k]].demanda;
                }

                /*Rota atual*/
                double consumo_atual = 0.0;
                double carga_tmp = carga_antes_i;
                for (int k = i; k <= j + 1; k++) {
                    carga_tmp += clientes[r->sequencia[k]].demanda;
                    double dist = distancia(clientes[r->sequencia[k-1]], clientes[r->sequencia[k]]);
                    consumo_atual += consumo_aresta(dist, carga_tmp);
                }

                /* Rota nova */ 
                double consumo_novo = 0.0;
                carga_tmp = carga_antes_i;

                carga_tmp += clientes[r->sequencia[j]].demanda;
                consumo_novo += consumo_aresta(distancia(clientes[r->sequencia[i-1]], clientes[r->sequencia[j]]), carga_tmp);

                for (int k = j - 1; k >= i; k--) {
                    carga_tmp += clientes[r->sequencia[k]].demanda;
                    double dist2 = distancia(clientes[r->sequencia[k+1]], clientes[r->sequencia[k]]);
                    consumo_novo += consumo_aresta(dist2, carga_tmp);
                }

                carga_tmp += clientes[r->sequencia[j+1]].demanda;
                consumo_novo += consumo_aresta(distancia(clientes[r->sequencia[i]], clientes[r->sequencia[j+1]]), carga_tmp);

                if (consumo_novo < consumo_atual - LIMIAR_MAX) {
                    int esq = i, dir = j;
                    while (esq < dir) {
                        int tmp = r->sequencia[esq];
                        r->sequencia[esq] = r->sequencia[dir];
                        r->sequencia[dir] = tmp;
                        esq++;
                        dir--;
                    }
                    recalcula_rota(r, clientes);
                    melhorou = 1;
                }
            }
        }
    }
}

/* PARTE 3 — METAHEURÍSTICA: ANT COLONY OPTIMIZATION (ACO)
    Referências:
    Dorigo, M. & Gambardella, L.M. (1997). Ant Colony System. IEEE Transactions on Evolutionary Computation, 1(1).

    Bullnheimer, B., Hartl, R.F. & Strauss, C. (1999). An Improved Ant System for the VRP. Annals of Operations Research.
*/

#define NUM_FORMIGAS 20 //formigas por iteração 
#define NUM_ITER 100 //iterações totais do loop de fora 
#define ALFA 1.0 //peso do feromônio na escolha probabilística
#define BETA 3.0 //peso da heurística na escolha
#define RHO 0.2 //taxa de evaporação do feromônio (0 a 1)
#define Q_FEROMONIO 1.0 //constante de escala do depósito de feromônio
#define TAU_INICIAL 0.1 //valor inicial de feromônio em todas as arestas

// intensidade dos feromonios por aresta (na matriz[origem][destino])
static double feromonio[NUM_CLIENTES][NUM_CLIENTES];

void inicializa_feromonio(int n) {
    for (int i = 0; i < n; i++){
        for (int j = 0; j < n; j++){
            feromonio[i][j] = TAU_INICIAL;
        }
    }
}

void atualiza_feromonio(Rota rotas[], int num_rotas, double custo_total, int n) {
    // Evaporação global do feromonio
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
            feromonio[i][j] *= (1.0 - RHO);

    //Depositando o feromonio conforme o custo de cada rota das formigas
    double delta = Q_FEROMONIO / custo_total;
    for (int v = 0; v < num_rotas; v++) {
        for (int k = 0; k < rotas[v].tamanho - 1; k++) {
            int a = rotas[v].sequencia[k];
            int b = rotas[v].sequencia[k + 1];
            feromonio[a][b] += delta;
            feromonio[b][a] += delta; 
        }
    }
}

/* ─────────────────────────────────────────────────────────────
   MECANISMO DE ESCOLHA:
   Para cada posição, a formiga calcula para todos os candidatos
   válidos (não visitados, que cabem na carga) um "atrativo":

     atrativo(j) = τ(pos,j)^α × η(pos,j)^β
   ───────────────────────────────────────────────────────────── */

//Func auxiliar pra copiar rotas de um array para outro
void copia_rotas(Rota destino[], Rota origem[], int num_rotas) {
    for (int v = 0; v < num_rotas; v++)
        destino[v] = origem[v];
}

// VER A PARTIR DAQUI SE N MUDEI OS PARAMETROS ERRADOS
void formiga_perturba(Rota origem[], int n_rotas, Rota destino[])
{
    copia_rotas(destino, origem, n_rotas);

    for (int r = 0; r < n_rotas; r++){
        if (destino[r].tamanho <= 4) {continue;}

        int i = 1 + rand() % (destino[r].tamanho - 2);
        int j = 1 + rand() % (destino[r].tamanho - 2);

        while (j == i)
            j = 1 + rand() % (destino[r].tamanho - 2);

        int aux = destino[r].sequencia[i];
        destino[r].sequencia[i] = destino[r].sequencia[j];
        destino[r].sequencia[j] = aux;
    }
}

int solucao_valida(Rota rotas[], int num_rotas,
                   Cliente clientes[],
                   int n,
                   double capacidade,
                   int max_veiculos)
{
    if (num_rotas > max_veiculos)
        return 0;

    int visitado[NUM_CLIENTES] = {0};

    visitado[0] = 1;

    for (int r = 0; r < num_rotas; r++)
    {
        double carga = 0.0;

        if (rotas[r].sequencia[0] != 0)
            return 0;

        if (rotas[r].sequencia[rotas[r].tamanho - 1] != 0)
            return 0;

        for (int i = 1; i < rotas[r].tamanho - 1; i++)
        {
            int c = rotas[r].sequencia[i];

            if (c <= 0 || c >= n)
                return 0;

            if (visitado[c])
                return 0;

            visitado[c] = 1;

            carga += clientes[c].demanda;
        }

        if (carga > capacidade + 1e-6)
            return 0;
    }

    for (int i = 1; i < n; i++)
    {
        if (!visitado[i])
            return 0;
    }

    return 1;
}

double aco_gvrp(Cliente clientes[], int num_clientes, double capacidade, Rota rotas_iniciais[], int n_rotas_iniciais, Rota melhor_rotas[], int *melhor_n_rotas){

    inicializa_feromonio(num_clientes);

    double melhor_consumo_global = INFINITY;
    Rota rotas_formiga[NUM_VEICULOS];
    Rota rotas_melhor_iter[NUM_VEICULOS];

    for (int iter = 0; iter < NUM_ITER; iter++) {
        double melhor_consumo_iter = INFINITY;
        int melhor_n_iter = 0;

        for (int f = 0; f < NUM_FORMIGAS; f++) {
            
            formiga_perturba(rotas_iniciais,n_rotas_iniciais, rotas_formiga);
            int n_rotas_f = n_rotas_iniciais;

            for (int v = 0; v < n_rotas_f; v++) {
                dois_opt(&rotas_formiga[v], clientes);
            }

            double consumo_f = 0.0;
            for (int v = 0; v < n_rotas_f; v++) {
                consumo_f += rotas_formiga[v].consumo_total;
            }

           /* printf("Formiga %d: rotas=%d\n", f + 1, n_rotas_f);
            if (!solucao_valida(rotas_formiga, n_rotas_f, clientes, num_clientes, capacidade, max_veiculos)){
                printf(" -> invalida\n");
                continue;
            }*/

            if (consumo_f < melhor_consumo_iter) {
                melhor_consumo_iter = consumo_f;
                melhor_n_iter = n_rotas_f;
                copia_rotas(rotas_melhor_iter, rotas_formiga, n_rotas_f);
            }

            if (consumo_f < melhor_consumo_global) {
                melhor_consumo_global = consumo_f;
                *melhor_n_rotas = n_rotas_f;
                copia_rotas(melhor_rotas, rotas_formiga, n_rotas_f);
                printf("iteracao: %3d, formiga: %2d novo melhor consumo: %.2f L\n", iter + 1, f + 1, consumo_f);
            }
        }
        atualiza_feromonio(rotas_melhor_iter, melhor_n_iter, melhor_consumo_iter, num_clientes);
    }
    return melhor_consumo_global;
}

int main(){
    Cliente clientes[NUM_CLIENTES]; //Array de clientes e cliente[0] é o depósito
    Rota rotas_gvrp[NUM_VEICULOS], rotas_vrp[NUM_VEICULOS]; //Array de rotas para GVRP e VRP
    int n; // Num de clientes (+ depósito)
    double capacidade; // Capacidade do veiculo

    ler_instancia("tai75d.vrp", clientes, &n, &capacidade);

    // Calcula K_min = ceil(soma_demandas / capacidade)
    //Menor num de veiculos - respeitando o |K| das instâncias TAI da CVRPLIB
    double soma_demandas = 0.0;
    for (int i = 1; i < n; i++){
        soma_demandas += clientes[i].demanda;
    }
    int k_min = (int)ceil(soma_demandas / capacidade);
    int max_veiculos = k_min;

    printf("Instancia: %d clientes | Capacidade: %.0f | ""Demanda total: %.0f | K_min: %d\n\n", n - 1, capacidade, soma_demandas, k_min);

    /*PARTE 1 do Trabalho: Guloso GVRP */
    int n_gvrp;
    clock_t t0 = clock();
    guloso_gvrp(clientes, n, capacidade, rotas_gvrp, &n_gvrp, 1, max_veiculos);
    clock_t t1 = clock();

    double consumo_gvrp_guloso = 0.0;
    for (int i = 0; i < n_gvrp; i++){
        consumo_gvrp_guloso += rotas_gvrp[i].consumo_total;
    }

    /*PARTE 2: Busca local 2-opt*/
    clock_t t2 = clock();
    for (int v = 0; v < n_gvrp; v++){
        dois_opt(&rotas_gvrp[v], clientes);
    }
    clock_t t3 = clock();

    double consumo_gvrp_bl = 0.0;
    for (int i = 0; i < n_gvrp; i++){
        consumo_gvrp_bl += rotas_gvrp[i].consumo_total;
    }

    /* VRP Guloso pra comparar */
    reset_visitados(clientes, n);
    int n_vrp;
    guloso_gvrp(clientes, n, capacidade, rotas_vrp, &n_vrp, 0, max_veiculos);

    int nao_visitados = 0;
    for (int i = 1; i < n; i++)
        if (!clientes[i].visitado) nao_visitados++;

    double consumo_vrp = 0.0;
    for (int i = 0; i < n_vrp; i++)
        consumo_vrp += rotas_vrp[i].consumo_total;

    /*  PARTE 3: ACO   */
    printf("Mostrando as formigas trabalhando: (%d formigas x %d iteracoes)\n", NUM_FORMIGAS, NUM_ITER);

    Rota rotas_aco[NUM_VEICULOS];
    int  n_aco = 0; 

    clock_t t4 = clock();
    double consumo_aco = aco_gvrp(clientes, n, capacidade, rotas_gvrp, n_gvrp, rotas_aco, &n_aco);
    clock_t t5 = clock();

    int aco_viola_capacidade = 0;
    for (int i = 0; i < n_aco; i++) {
        if (rotas_aco[i].carga_atual > capacidade + 1e-6) {
            aco_viola_capacidade = 1;
            break;
        }
    }

    /*  Métricas  */
    double tempo_guloso = (double)(t1 - t0)/CLOCKS_PER_SEC;
    double tempo_bl = (double)(t3 - t2)/CLOCKS_PER_SEC;
    double tempo_aco = (double)(t5 - t4)/CLOCKS_PER_SEC;

    double melhoria_bl = 100.0 * (consumo_gvrp_guloso - consumo_gvrp_bl)/consumo_gvrp_guloso;
    double melhoria_aco_guloso = 100.0 * (consumo_gvrp_guloso - consumo_aco)/consumo_gvrp_guloso;
    double melhoria_aco_bl = 100.0 * (consumo_gvrp_bl - consumo_aco)/consumo_gvrp_bl;
    double economia_bl_vrp = 100.0 * (consumo_vrp - consumo_gvrp_bl)/consumo_vrp;
    double economia_aco_vrp = 100.0 * (consumo_vrp - consumo_aco)/consumo_vrp;

    //Correção do algoritmo (da parte 1 e 2 do trabalho) para garantir o número correto de veículos utilizados
    const char *v_guloso = (n_gvrp <= max_veiculos) ? "OK" : "INVIAVEL";
    const char *v_bl = (n_gvrp <= max_veiculos) ? "OK" : "INVIAVEL";
    const char *v_aco = (n_aco <= max_veiculos && !aco_viola_capacidade) ? "OK" : "INVIAVEL";

    double distancia_vrp = 0.0;
    for (int i = 0; i < n_vrp; i++){
        distancia_vrp += rotas_vrp[i].distancia_total;
    }

    double distancia_gvrp_guloso = 0.0;
    for (int i = 0; i < n_gvrp; i++){
        distancia_gvrp_guloso += rotas_gvrp[i].distancia_total;
    }

    double distancia_gvrp_bl = 0.0;
    for (int i = 0; i < n_gvrp; i++) {
        distancia_gvrp_bl += rotas_gvrp[i].distancia_total;
    }

    double distancia_aco = 0.0;
    for (int i = 0; i < n_aco; i++) {
        distancia_aco += rotas_aco[i].distancia_total;
    }

    printf("\nClientes nao visitados: %d\n\n", nao_visitados);
    printf("\nValidacao da solucao ACO\n");
    printf("Rotas: %d\n", n_aco);
    printf("Capacidade respeitada: %s\n", aco_viola_capacidade ? "NAO" : "SIM");
    printf("=== RESULTADOS ===\n\n");
    printf("| VRP  Guloso  | Distancia: %8.2f km | Combustivel: %8.2f L | Veiculos: %2d/%d |\n", distancia_vrp, consumo_vrp, n_vrp, max_veiculos);
    printf("| GVRP Guloso  | Distancia: %8.2f km | Combustivel: %8.2f L | Veiculos: %2d/%d [%s] | Tempo: %.4fs |\n", distancia_gvrp_guloso, consumo_gvrp_guloso, n_gvrp, max_veiculos, v_guloso, tempo_guloso);
    printf("| GVRP + 2-opt | Distancia: %8.2f km | Combustivel: %8.2f L | Veiculos: %2d/%d [%s] | Tempo: %.4fs | Melhoria vs guloso: %.2f%% |\n", distancia_gvrp_bl, consumo_gvrp_bl, n_gvrp, max_veiculos, v_bl, tempo_bl, melhoria_bl);
    printf("| GVRP + ACO   | Distancia: %8.2f km | Combustivel: %8.2f L | Veiculos: %2d/%d [%s] | Tempo: %.4fs | Melhoria vs guloso: %.2f%% | Melhoria vs 2-opt: %.2f%% |\n", distancia_aco, consumo_aco, n_aco, max_veiculos, v_aco, tempo_aco, melhoria_aco_guloso, melhoria_aco_bl);
    printf("\nEconomia (GVRP + 2-opt) vs VRP : %.2f%%\n", economia_bl_vrp);
    printf("Economia (GVRP + ACO)   vs VRP : %.2f%%\n", economia_aco_vrp);

    return 0;
}