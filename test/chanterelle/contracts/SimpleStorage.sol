pragma solidity ^0.6.12;

contract SimpleStorage {
    uint public count;
    
    event CountSet(uint _count);

    event Deployed(uint _blockNumber);

    constructor() public {
      emit Deployed(block.number);
    }
    
    function setCount(uint _count) public {
        count = _count;
        emit CountSet(_count);
    }
}
