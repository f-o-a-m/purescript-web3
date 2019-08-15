pragma solidity ^0.4.21;

contract Adoption {

  address[1] public adopters1;
  address[10] public adopters2;
  address[123456] public adopters3;

  function getAdoptersOneDigit() public view returns (address[1]) {
    return adopters1;
  }
  function getAdoptersTwoDigit() public view returns (address[10]) {
    return adopters2;
  }
  function getAdoptersManyDigit() public view returns (address[123456]) {
    return adopters3;
  }

}
