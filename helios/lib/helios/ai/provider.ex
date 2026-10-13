defmodule Helios.AI.Provider do
  @moduledoc """
  An AI provider adapter. `decide/3` answers one decision before the absolute
  `deadline` (monotonic milliseconds) with one of the prompt's move ids;
  `probe/1` checks the provider is configured and reachable and returns the
  ids of the given models that are ready.
  """

  @type usage :: %{input_tokens: non_neg_integer(), output_tokens: non_neg_integer()}

  @callback decide(model :: Helios.AI.Models.model(), prompt :: map(), deadline :: integer()) ::
              {:ok, String.t(), usage()} | {:error, term()}

  @callback probe(models :: [Helios.AI.Models.model()]) ::
              {:ok, [String.t()]} | {:error, term()}
end
