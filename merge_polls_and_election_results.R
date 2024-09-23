merge_polls_and_election_results = function(p, votes) {
  p %>%
    group_by(NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, polled_UE, estimulada, CD_CARGO, vv) %>%
    filter(n_distinct(scenario_id) == 1) %>%
    ungroup() %>%
    inner_join(votes, by = c(
      'year' = 'ANO_ELEICAO',
      'turno' = 'NUM_TURNO',
      'polled_UE' = 'SIGLA_UE',
      'CD_CARGO' = 'CODIGO_CARGO',
      'NUMERO_CANDIDATO' = 'NUMERO_CANDIDATO'
    )) %>%
    mutate(state = case_when(
      polled_UE == 'BR' ~ 'BR',
      nchar(polled_UE) == 2 ~ polled_UE,
      T ~ str_sub(NR_IDENTIFICACAO_PESQUISA, start = 1, end = 2)
    )) %>%
    group_by(year, turno,
             NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, polled_UE, CD_CARGO, estimulada, vv, scenario_id,
             DT_INICIO_PESQUISA, DT_FIM_PESQUISA,  is_phone, is_fluxo, QT_ENTREVISTADOS,
             first_round_date, second_round_date, is_complete, state,
             confidence_interval_final, error_final) %>%
    mutate(valid_result = result / sum(result) * 100) %>%
    mutate(pct = QTDE_VOTOS / sum(QTDE_VOTOS) * 100) %>%
    filter(sum(QTDE_VOTOS) >= 0.90 * qtde_all_valid) %>%
    mutate(undecided = 100 - sum(result)) %>%
    ungroup()
}
