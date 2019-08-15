pragma solidity ^0.4.15;

contract SimpleStorage {
    uint public count;
    
    event CountSet(uint _count);

    event Deployed(uint _blockNumber);

    function SimpleStorage() {
      Deployed(block.number);
    }
    
    function setCount(uint _count) {
        count = _count;
        CountSet(_count);
    }
}
