import pandas as pd
import requests
import zipfile
import pathlib
import os
import io

def comparecimento():
    dados = {
        f'perfil_comparecimento_abstencao_{ano}.csv': f'https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_comparecimento_abstencao/perfil_comparecimento_abstencao_{ano}.zip' for ano in [2014, 2018, 2022]}

    dir = pathlib.Path.cwd() / 'dados/comparecimento'
    dir.mkdir(exist_ok=True, parents=True)

    for nome_arquivo, url in dados.items():
        path_dados = dir / nome_arquivo

        if not path_dados.exists():
            print(f'Fazendo download de {nome_arquivo}')
            response = requests.get(url)

            with zipfile.ZipFile(io.BytesIO(response.content), 'r') as zip_ref:
                with zip_ref.open(nome_arquivo) as csv_file:
                    (pd.read_csv(csv_file, encoding = "latin", sep = ";")
                        .groupby(["ANO_ELEICAO", "NR_TURNO", "CD_MUNICIPIO", "DS_FAIXA_ETARIA"])
                        .agg({col: "sum" for col in ["QT_APTOS", "QT_COMPARECIMENTO", "QT_ABSTENCAO"]})
                        .reset_index()
                        .to_csv(path_dados, index=False)
                    )

            print(f'{nome_arquivo} foi salvo')
        else: print(f'{nome_arquivo} já existe')

def resultado():
    dados = {
        f'votacao_candidato_munzona_{ano}_BRASIL.csv': f'https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_{ano}.zip' for ano in [2014, 2018, 2022]}

    dir = pathlib.Path.cwd() / 'dados/resultado'
    dir.mkdir(exist_ok=True, parents=True)

    for nome_arquivo, url in dados.items():
        path_dados = dir / nome_arquivo

        if not path_dados.exists():
            print(f'Fazendo download de {nome_arquivo} \nO arquivo é grande, então pode demorar')
            response = requests.get(url)

            with zipfile.ZipFile(io.BytesIO(response.content), 'r') as zip_ref:
                with zip_ref.open(nome_arquivo) as csv_file:
                    (
                        pd.read_csv(
                            csv_file, encoding = "latin", sep = ";",
                            usecols = ["ANO_ELEICAO", "NR_TURNO", "CD_MUNICIPIO", "SG_UF", "DS_CARGO", "DS_SIT_TOT_TURNO", "QT_VOTOS_NOMINAIS"])
                        .query("NR_TURNO == 2 and DS_CARGO == 'Presidente' and SG_UF != 'ZZ'")
                        .to_csv(path_dados, index=False)
                    )

            print(f'{nome_arquivo} foi salvo')
        else: print(f'{nome_arquivo} já existe')

comparecimento()
resultado()