unit SapiensTypes;

interface

type
	TContentType = (ctString, ctInteger, ctReal, ctDate);
  TOperator = (opOr, opAnd, opNull);
  TCriteriaType = (	tcEquals,
  									tcNotEqual,
                    tcGreaterThen,
                    tcGreaterThanOrEqual,
                    tcLessThan,
                    tcLessThanOrEqual,
                    tcNull,
                    tcNotNull,
                    tcStartingWith,
                    tcContains);
const
	CRITERIAS_FOR_ALL : Set of TCriteriaType = [tcEquals,
  																						tcNotEqual,
                    													tcGreaterThen,
                    													tcGreaterThanOrEqual,
                    													tcLessThan,
                    													tcLessThanOrEqual,
                    													tcNull,
                    													tcNotNull];
	CRITERIAS_ONLY_TO_STRING : Set of TCriteriaType = [tcStartingWith, tcContains];

	CRITERIAS_NAMES : array[TCriteriaType] of String	  =
  								(	'igual a',
  									'diferente de',
                    'maior que',
                    'maior ou igual a',
                    'menor que',
                    'menor ou igual a',
                    'é nulo',
                    'diferente de nulo',
                    'iniciando com',
                    'contendo');
	CRITERIAS_SYMBOLS : array[TCriteriaType] of String	  =
  								(	'=',
  									'<>',
                    '>',
                    '>=',
                    '<',
                    '<=',
                    'IS NULL',
                    'IS NOT NULL',
                    'STARTING WITH',
                    'CONTAINS');

implementation

end.
