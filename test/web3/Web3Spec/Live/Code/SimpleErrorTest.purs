module Web3Spec.Live.Code.SimpleErrorTest where

import Network.Ethereum.Web3 (HexString)
import Web3Spec.Live.Utils (mkHexString')

deployBytecode :: HexString
deployBytecode =
  mkHexString'
    "608060405234801561001057600080fd5b50610385806100206000396000f3fe608060405234801561001057600080fd5b50600436106100415760003560e01c80634622ab0314610046578063bafff8c0146100ed578063e8dde23214610194575b600080fd5b6100726004803603602081101561005c57600080fd5b81019080803590602001909291905050506101dc565b6040518080602001828103825283818151815260200191508051906020019080838360005b838110156100b2578082015181840152602081019050610097565b50505050905090810190601f1680156100df5780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b6101196004803603602081101561010357600080fd5b8101908080359060200190929190505050610295565b6040518080602001828103825283818151815260200191508051906020019080838360005b8381101561015957808201518184015260208101905061013e565b50505050905090810190601f1680156101865780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b6101c2600480360360208110156101aa57600080fd5b81019080803515159060200190929190505050610345565b604051808215151515815260200191505060405180910390f35b600181815481106101e957fe5b906000526020600020016000915090508054600181600116156101000203166002900480601f01602080910402602001604051908101604052809291908181526020018280546001816001161561010002031660029004801561028d5780601f106102625761010080835404028352916020019161028d565b820191906000526020600020905b81548152906001019060200180831161027057829003601f168201915b505050505081565b60006020528060005260406000206000915090508054600181600116156101000203166002900480601f01602080910402602001604051908101604052809291908181526020018280546001816001161561010002031660029004801561033d5780601f106103125761010080835404028352916020019161033d565b820191906000526020600020905b81548152906001019060200180831161032057829003601f168201915b505050505081565b60008115905091905056fea265627a7a72315820dab93eda565cb9057d5b1be8ec881b24251fc5d67e14beddc0563abcc710254164736f6c63430005110032"
