// mod.rs

// *************************************************************************
// * Copyright (C) 2019 Daniel Mueller (deso@posteo.net)                   *
// *                                                                       *
// * This program is free software: you can redistribute it and/or modify  *
// * it under the terms of the GNU General Public License as published by  *
// * the Free Software Foundation, either version 3 of the License, or     *
// * (at your option) any later version.                                   *
// *                                                                       *
// * This program is distributed in the hope that it will be useful,       *
// * but WITHOUT ANY WARRANTY; without even the implied warranty of        *
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
// * GNU General Public License for more details.                          *
// *                                                                       *
// * You should have received a copy of the GNU General Public License     *
// * along with this program.  If not, see <http://www.gnu.org/licenses/>. *
// *************************************************************************

use nitrokey::CommunicationError;
use nitrokey::Device;
use nitrokey::Error;
use nitrokey::Model;


#[nitrokey_test::test]
fn no_dev() {
  let error = nitrokey::connect().unwrap_err();
  match error {
    Error::CommunicationError(CommunicationError::NotConnected) => (),
    _ => panic!("received unexpected error: {:?}", error),
  }
}

#[nitrokey_test::test]
fn pro(device: Pro) {
  assert_eq!(device.get_model(), Model::Pro);
  drop(device);

  assert!(nitrokey::connect_model(Model::Pro).is_ok())
}

#[nitrokey_test::test]
fn storage(device: Storage) {
  assert_eq!(device.get_model(), Model::Storage);
  drop(device);

  assert!(nitrokey::connect_model(Model::Storage).is_ok())
}

#[nitrokey_test::test]
fn any(device: DeviceWrapper) {
  let model = device.get_model();
  drop(device);

  assert!(nitrokey::connect_model(model).is_ok())
}

#[nitrokey_test::test]
#[ignore]
fn ignore_no_dev() {
  panic!("should be ignored")
}

#[nitrokey_test::test]
#[ignore]
fn ignore_any(_device: nitrokey::DeviceWrapper) {
  panic!("should be ignored")
}


/// A trait providing a method with a &mut self signature.
trait MutableDevice {
  fn test_mut(&mut self) -> bool {
    true
  }
}

impl MutableDevice for nitrokey::DeviceWrapper {}

#[nitrokey_test::test]
fn mutable_device(mut device: nitrokey::DeviceWrapper) {
  assert!(device.test_mut())
}

#[nitrokey_test::test]
fn aribtrary_argument_name(foobarbaz: nitrokey::DeviceWrapper) {
  let _ = foobarbaz.get_model();
}
